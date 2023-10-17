use collections::{HashMap, VecDeque};
use editor::{Editor, MoveToEnd};
use futures::{channel::mpsc, StreamExt};
use gpui::{
    actions,
    elements::{
        AnchorCorner, ChildView, Empty, Flex, Label, MouseEventHandler, Overlay, OverlayFitMode,
        ParentElement, Stack,
    },
    platform::{CursorStyle, MouseButton},
    AnyElement, AppContext, Element, Entity, ModelContext, ModelHandle, Subscription, View,
    ViewContext, ViewHandle, WeakModelHandle,
};
use language::{LanguageServerId, LanguageServerName};
use lsp::IoKind;
use project::{search::SearchQuery, Project};
use std::{borrow::Cow, sync::Arc};
use theme::{ui, Theme};
use workspace::{
    item::{Item, ItemHandle},
    searchable::{SearchableItem, SearchableItemHandle},
    ToolbarItemLocation, ToolbarItemView, Workspace, WorkspaceCreated,
};

const SEND_LINE: &str = "// Send:";
const RECEIVE_LINE: &str = "// Receive:";
const MAX_STORED_LOG_ENTRIES: usize = 2000;

pub struct LogStore {
    projects: HashMap<WeakModelHandle<Project>, ProjectState>,
    io_tx: mpsc::UnboundedSender<(WeakModelHandle<Project>, LanguageServerId, IoKind, String)>,
}

struct ProjectState {
    servers: HashMap<LanguageServerId, LanguageServerState>,
    _subscriptions: [gpui::Subscription; 2],
}

struct LanguageServerState {
    log_messages: VecDeque<String>,
    rpc_state: Option<LanguageServerRpcState>,
    _io_logs_subscription: Option<lsp::Subscription>,
    _lsp_logs_subscription: Option<lsp::Subscription>,
}

struct LanguageServerRpcState {
    rpc_messages: VecDeque<String>,
    last_message_kind: Option<MessageKind>,
}

pub struct LspLogView {
    pub(crate) editor: ViewHandle<Editor>,
    editor_subscription: Subscription,
    log_store: ModelHandle<LogStore>,
    current_server_id: Option<LanguageServerId>,
    is_showing_rpc_trace: bool,
    project: ModelHandle<Project>,
    _log_store_subscriptions: Vec<Subscription>,
}

pub struct LspLogToolbarItemView {
    log_view: Option<ViewHandle<LspLogView>>,
    _log_view_subscription: Option<Subscription>,
    menu_open: bool,
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum MessageKind {
    Send,
    Receive,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct LogMenuItem {
    pub server_id: LanguageServerId,
    pub server_name: LanguageServerName,
    pub worktree_root_name: String,
    pub rpc_trace_enabled: bool,
    pub rpc_trace_selected: bool,
    pub logs_selected: bool,
}

actions!(debug, [OpenLanguageServerLogs]);

pub fn init(cx: &mut AppContext) {
    let log_store = cx.add_model(|cx| LogStore::new(cx));

    cx.subscribe_global::<WorkspaceCreated, _>({
        let log_store = log_store.clone();
        move |event, cx| {
            let workspace = &event.0;
            if let Some(workspace) = workspace.upgrade(cx) {
                let project = workspace.read(cx).project().clone();
                if project.read(cx).is_local() {
                    log_store.update(cx, |store, cx| {
                        store.add_project(&project, cx);
                    });
                }
            }
        }
    })
    .detach();

    cx.add_action(
        move |workspace: &mut Workspace, _: &OpenLanguageServerLogs, cx: _| {
            let project = workspace.project().read(cx);
            if project.is_local() {
                workspace.add_item(
                    Box::new(cx.add_view(|cx| {
                        LspLogView::new(workspace.project().clone(), log_store.clone(), cx)
                    })),
                    cx,
                );
            }
        },
    );
}

impl LogStore {
    pub fn new(cx: &mut ModelContext<Self>) -> Self {
        let (io_tx, mut io_rx) = mpsc::unbounded();
        let this = Self {
            projects: HashMap::default(),
            io_tx,
        };
        cx.spawn_weak(|this, mut cx| async move {
            while let Some((project, server_id, io_kind, message)) = io_rx.next().await {
                if let Some(this) = this.upgrade(&cx) {
                    this.update(&mut cx, |this, cx| {
                        this.on_io(project, server_id, io_kind, &message, cx);
                    });
                }
            }
            anyhow::Ok(())
        })
        .detach();
        this
    }

    pub fn add_project(&mut self, project: &ModelHandle<Project>, cx: &mut ModelContext<Self>) {
        let weak_project = project.downgrade();
        self.projects.insert(
            weak_project,
            ProjectState {
                servers: HashMap::default(),
                _subscriptions: [
                    cx.observe_release(&project, move |this, _, _| {
                        this.projects.remove(&weak_project);
                    }),
                    cx.subscribe(project, |this, project, event, cx| match event {
                        project::Event::LanguageServerAdded(id) => {
                            this.add_language_server(&project, *id, cx);
                        }
                        project::Event::LanguageServerRemoved(id) => {
                            this.remove_language_server(&project, *id, cx);
                        }
                        project::Event::LanguageServerLog(id, message) => {
                            this.add_language_server_log(&project, *id, message, cx);
                        }
                        _ => {}
                    }),
                ],
            },
        );
    }

    fn add_language_server(
        &mut self,
        project: &ModelHandle<Project>,
        id: LanguageServerId,
        cx: &mut ModelContext<Self>,
    ) -> Option<&mut LanguageServerState> {
        let project_state = self.projects.get_mut(&project.downgrade())?;
        let server_state = project_state.servers.entry(id).or_insert_with(|| {
            cx.notify();
            LanguageServerState {
                rpc_state: None,
                log_messages: VecDeque::with_capacity(MAX_STORED_LOG_ENTRIES),
                _io_logs_subscription: None,
                _lsp_logs_subscription: None,
            }
        });

        let server = project.read(cx).language_server_for_id(id);
        if let Some(server) = server.as_deref() {
            if server.has_notification_handler::<lsp::notification::LogMessage>() {
                // Another event wants to re-add the server that was already added and subscribed to, avoid doing it again.
                return Some(server_state);
            }
        }

        let weak_project = project.downgrade();
        let io_tx = self.io_tx.clone();
        server_state._io_logs_subscription = server.as_ref().map(|server| {
            server.on_io(move |io_kind, message| {
                io_tx
                    .unbounded_send((weak_project, id, io_kind, message.to_string()))
                    .ok();
            })
        });
        let this = cx.weak_handle();
        let weak_project = project.downgrade();
        server_state._lsp_logs_subscription = server.map(|server| {
            let server_id = server.server_id();
            server.on_notification::<lsp::notification::LogMessage, _>({
                move |params, mut cx| {
                    if let Some((project, this)) =
                        weak_project.upgrade(&mut cx).zip(this.upgrade(&mut cx))
                    {
                        this.update(&mut cx, |this, cx| {
                            this.add_language_server_log(&project, server_id, &params.message, cx);
                        });
                    }
                }
            })
        });
        Some(server_state)
    }

    fn add_language_server_log(
        &mut self,
        project: &ModelHandle<Project>,
        id: LanguageServerId,
        message: &str,
        cx: &mut ModelContext<Self>,
    ) -> Option<()> {
        let language_server_state = match self
            .projects
            .get_mut(&project.downgrade())?
            .servers
            .get_mut(&id)
        {
            Some(existing_state) => existing_state,
            None => self.add_language_server(&project, id, cx)?,
        };

        let log_lines = &mut language_server_state.log_messages;
        while log_lines.len() >= MAX_STORED_LOG_ENTRIES {
            log_lines.pop_front();
        }
        let message = message.trim();
        log_lines.push_back(message.to_string());
        cx.emit(Event::NewServerLogEntry {
            id,
            entry: message.to_string(),
            is_rpc: false,
        });
        cx.notify();
        Some(())
    }

    fn remove_language_server(
        &mut self,
        project: &ModelHandle<Project>,
        id: LanguageServerId,
        cx: &mut ModelContext<Self>,
    ) -> Option<()> {
        let project_state = self.projects.get_mut(&project.downgrade())?;
        project_state.servers.remove(&id);
        cx.notify();
        Some(())
    }

    fn server_logs(
        &self,
        project: &ModelHandle<Project>,
        server_id: LanguageServerId,
    ) -> Option<&VecDeque<String>> {
        let weak_project = project.downgrade();
        let project_state = self.projects.get(&weak_project)?;
        let server_state = project_state.servers.get(&server_id)?;
        Some(&server_state.log_messages)
    }

    fn enable_rpc_trace_for_language_server(
        &mut self,
        project: &ModelHandle<Project>,
        server_id: LanguageServerId,
    ) -> Option<&mut LanguageServerRpcState> {
        let weak_project = project.downgrade();
        let project_state = self.projects.get_mut(&weak_project)?;
        let server_state = project_state.servers.get_mut(&server_id)?;
        let rpc_state = server_state
            .rpc_state
            .get_or_insert_with(|| LanguageServerRpcState {
                rpc_messages: VecDeque::with_capacity(MAX_STORED_LOG_ENTRIES),
                last_message_kind: None,
            });
        Some(rpc_state)
    }

    pub fn disable_rpc_trace_for_language_server(
        &mut self,
        project: &ModelHandle<Project>,
        server_id: LanguageServerId,
        _: &mut ModelContext<Self>,
    ) -> Option<()> {
        let project = project.downgrade();
        let project_state = self.projects.get_mut(&project)?;
        let server_state = project_state.servers.get_mut(&server_id)?;
        server_state.rpc_state.take();
        Some(())
    }

    fn on_io(
        &mut self,
        project: WeakModelHandle<Project>,
        language_server_id: LanguageServerId,
        io_kind: IoKind,
        message: &str,
        cx: &mut ModelContext<Self>,
    ) -> Option<()> {
        let is_received = match io_kind {
            IoKind::StdOut => true,
            IoKind::StdIn => false,
            IoKind::StdErr => {
                let project = project.upgrade(cx)?;
                let message = format!("stderr: {}", message.trim());
                self.add_language_server_log(&project, language_server_id, &message, cx);
                return Some(());
            }
        };

        let state = self
            .projects
            .get_mut(&project)?
            .servers
            .get_mut(&language_server_id)?
            .rpc_state
            .as_mut()?;
        let kind = if is_received {
            MessageKind::Receive
        } else {
            MessageKind::Send
        };

        let rpc_log_lines = &mut state.rpc_messages;
        if state.last_message_kind != Some(kind) {
            let line_before_message = match kind {
                MessageKind::Send => SEND_LINE,
                MessageKind::Receive => RECEIVE_LINE,
            };
            rpc_log_lines.push_back(line_before_message.to_string());
            cx.emit(Event::NewServerLogEntry {
                id: language_server_id,
                entry: line_before_message.to_string(),
                is_rpc: true,
            });
        }

        while rpc_log_lines.len() >= MAX_STORED_LOG_ENTRIES {
            rpc_log_lines.pop_front();
        }
        let message = message.trim();
        rpc_log_lines.push_back(message.to_string());
        cx.emit(Event::NewServerLogEntry {
            id: language_server_id,
            entry: message.to_string(),
            is_rpc: true,
        });
        cx.notify();
        Some(())
    }
}

impl LspLogView {
    pub fn new(
        project: ModelHandle<Project>,
        log_store: ModelHandle<LogStore>,
        cx: &mut ViewContext<Self>,
    ) -> Self {
        let server_id = log_store
            .read(cx)
            .projects
            .get(&project.downgrade())
            .and_then(|project| project.servers.keys().copied().next());
        let model_changes_subscription = cx.observe(&log_store, |this, store, cx| {
            (|| -> Option<()> {
                let project_state = store.read(cx).projects.get(&this.project.downgrade())?;
                if let Some(current_lsp) = this.current_server_id {
                    if !project_state.servers.contains_key(&current_lsp) {
                        if let Some(server) = project_state.servers.iter().next() {
                            if this.is_showing_rpc_trace {
                                this.show_rpc_trace_for_server(*server.0, cx)
                            } else {
                                this.show_logs_for_server(*server.0, cx)
                            }
                        } else {
                            this.current_server_id = None;
                            this.editor.update(cx, |editor, cx| {
                                editor.set_read_only(false);
                                editor.clear(cx);
                                editor.set_read_only(true);
                            });
                            cx.notify();
                        }
                    }
                } else {
                    if let Some(server) = project_state.servers.iter().next() {
                        if this.is_showing_rpc_trace {
                            this.show_rpc_trace_for_server(*server.0, cx)
                        } else {
                            this.show_logs_for_server(*server.0, cx)
                        }
                    }
                }

                Some(())
            })();

            cx.notify();
        });
        let events_subscriptions = cx.subscribe(&log_store, |log_view, _, e, cx| match e {
            Event::NewServerLogEntry { id, entry, is_rpc } => {
                if log_view.current_server_id == Some(*id) {
                    if (*is_rpc && log_view.is_showing_rpc_trace)
                        || (!*is_rpc && !log_view.is_showing_rpc_trace)
                    {
                        log_view.editor.update(cx, |editor, cx| {
                            editor.set_read_only(false);
                            editor.handle_input(entry.trim(), cx);
                            editor.handle_input("\n", cx);
                            editor.set_read_only(true);
                        });
                    }
                }
            }
        });
        let (editor, editor_subscription) = Self::editor_for_logs(String::new(), cx);
        let mut this = Self {
            editor,
            editor_subscription,
            project,
            log_store,
            current_server_id: None,
            is_showing_rpc_trace: false,
            _log_store_subscriptions: vec![model_changes_subscription, events_subscriptions],
        };
        if let Some(server_id) = server_id {
            this.show_logs_for_server(server_id, cx);
        }
        this
    }

    fn editor_for_logs(
        log_contents: String,
        cx: &mut ViewContext<Self>,
    ) -> (ViewHandle<Editor>, Subscription) {
        let editor = cx.add_view(|cx| {
            let mut editor = Editor::multi_line(None, cx);
            editor.set_text(log_contents, cx);
            editor.move_to_end(&MoveToEnd, cx);
            editor.set_read_only(true);
            editor
        });
        let editor_subscription = cx.subscribe(&editor, |_, _, event, cx| cx.emit(event.clone()));
        (editor, editor_subscription)
    }

    pub(crate) fn menu_items<'a>(&'a self, cx: &'a AppContext) -> Option<Vec<LogMenuItem>> {
        let log_store = self.log_store.read(cx);
        let state = log_store.projects.get(&self.project.downgrade())?;
        let mut rows = self
            .project
            .read(cx)
            .language_servers()
            .filter_map(|(server_id, language_server_name, worktree_id)| {
                let worktree = self.project.read(cx).worktree_for_id(worktree_id, cx)?;
                let state = state.servers.get(&server_id)?;
                Some(LogMenuItem {
                    server_id,
                    server_name: language_server_name,
                    worktree_root_name: worktree.read(cx).root_name().to_string(),
                    rpc_trace_enabled: state.rpc_state.is_some(),
                    rpc_trace_selected: self.is_showing_rpc_trace
                        && self.current_server_id == Some(server_id),
                    logs_selected: !self.is_showing_rpc_trace
                        && self.current_server_id == Some(server_id),
                })
            })
            .chain(
                self.project
                    .read(cx)
                    .supplementary_language_servers()
                    .filter_map(|(&server_id, (name, _))| {
                        let state = state.servers.get(&server_id)?;
                        Some(LogMenuItem {
                            server_id,
                            server_name: name.clone(),
                            worktree_root_name: "supplementary".to_string(),
                            rpc_trace_enabled: state.rpc_state.is_some(),
                            rpc_trace_selected: self.is_showing_rpc_trace
                                && self.current_server_id == Some(server_id),
                            logs_selected: !self.is_showing_rpc_trace
                                && self.current_server_id == Some(server_id),
                        })
                    }),
            )
            .collect::<Vec<_>>();
        rows.sort_by_key(|row| row.server_id);
        rows.dedup_by_key(|row| row.server_id);
        Some(rows)
    }

    fn show_logs_for_server(&mut self, server_id: LanguageServerId, cx: &mut ViewContext<Self>) {
        let log_contents = self
            .log_store
            .read(cx)
            .server_logs(&self.project, server_id)
            .map(log_contents);
        if let Some(log_contents) = log_contents {
            self.current_server_id = Some(server_id);
            self.is_showing_rpc_trace = false;
            let (editor, editor_subscription) = Self::editor_for_logs(log_contents, cx);
            self.editor = editor;
            self.editor_subscription = editor_subscription;
            cx.notify();
        }
    }

    fn show_rpc_trace_for_server(
        &mut self,
        server_id: LanguageServerId,
        cx: &mut ViewContext<Self>,
    ) {
        let rpc_log = self.log_store.update(cx, |log_store, _| {
            log_store
                .enable_rpc_trace_for_language_server(&self.project, server_id)
                .map(|state| log_contents(&state.rpc_messages))
        });
        if let Some(rpc_log) = rpc_log {
            self.current_server_id = Some(server_id);
            self.is_showing_rpc_trace = true;
            let (editor, editor_subscription) = Self::editor_for_logs(rpc_log, cx);
            let language = self.project.read(cx).languages().language_for_name("JSON");
            editor
                .read(cx)
                .buffer()
                .read(cx)
                .as_singleton()
                .expect("log buffer should be a singleton")
                .update(cx, |_, cx| {
                    cx.spawn_weak({
                        let buffer = cx.handle();
                        |_, mut cx| async move {
                            let language = language.await.ok();
                            buffer.update(&mut cx, |buffer, cx| {
                                buffer.set_language(language, cx);
                            });
                        }
                    })
                    .detach();
                });

            self.editor = editor;
            self.editor_subscription = editor_subscription;
            cx.notify();
        }
    }

    fn toggle_rpc_trace_for_server(
        &mut self,
        server_id: LanguageServerId,
        enabled: bool,
        cx: &mut ViewContext<Self>,
    ) {
        self.log_store.update(cx, |log_store, cx| {
            if enabled {
                log_store.enable_rpc_trace_for_language_server(&self.project, server_id);
            } else {
                log_store.disable_rpc_trace_for_language_server(&self.project, server_id, cx);
            }
        });
        if !enabled && Some(server_id) == self.current_server_id {
            self.show_logs_for_server(server_id, cx);
            cx.notify();
        }
    }
}

fn log_contents(lines: &VecDeque<String>) -> String {
    let (a, b) = lines.as_slices();
    let log_contents = a.join("\n");
    if b.is_empty() {
        log_contents
    } else {
        log_contents + "\n" + &b.join("\n")
    }
}

impl View for LspLogView {
    fn ui_name() -> &'static str {
        "LspLogView"
    }

    fn render(&mut self, cx: &mut ViewContext<Self>) -> AnyElement<Self> {
        ChildView::new(&self.editor, cx).into_any()
    }

    fn focus_in(&mut self, _: gpui::AnyViewHandle, cx: &mut ViewContext<Self>) {
        if cx.is_self_focused() {
            cx.focus(&self.editor);
        }
    }
}

impl Item for LspLogView {
    fn tab_content<V: 'static>(
        &self,
        _: Option<usize>,
        style: &theme::Tab,
        _: &AppContext,
    ) -> AnyElement<V> {
        Label::new("LSP Logs", style.label.clone()).into_any()
    }

    fn as_searchable(&self, handle: &ViewHandle<Self>) -> Option<Box<dyn SearchableItemHandle>> {
        Some(Box::new(handle.clone()))
    }
}

impl SearchableItem for LspLogView {
    type Match = <Editor as SearchableItem>::Match;

    fn to_search_event(
        &mut self,
        event: &Self::Event,
        cx: &mut ViewContext<Self>,
    ) -> Option<workspace::searchable::SearchEvent> {
        self.editor
            .update(cx, |editor, cx| editor.to_search_event(event, cx))
    }

    fn clear_matches(&mut self, cx: &mut ViewContext<Self>) {
        self.editor.update(cx, |e, cx| e.clear_matches(cx))
    }

    fn update_matches(&mut self, matches: Vec<Self::Match>, cx: &mut ViewContext<Self>) {
        self.editor
            .update(cx, |e, cx| e.update_matches(matches, cx))
    }

    fn query_suggestion(&mut self, cx: &mut ViewContext<Self>) -> String {
        self.editor.update(cx, |e, cx| e.query_suggestion(cx))
    }

    fn activate_match(
        &mut self,
        index: usize,
        matches: Vec<Self::Match>,
        cx: &mut ViewContext<Self>,
    ) {
        self.editor
            .update(cx, |e, cx| e.activate_match(index, matches, cx))
    }

    fn select_matches(&mut self, matches: Vec<Self::Match>, cx: &mut ViewContext<Self>) {
        self.editor
            .update(cx, |e, cx| e.select_matches(matches, cx))
    }

    fn find_matches(
        &mut self,
        query: Arc<project::search::SearchQuery>,
        cx: &mut ViewContext<Self>,
    ) -> gpui::Task<Vec<Self::Match>> {
        self.editor.update(cx, |e, cx| e.find_matches(query, cx))
    }

    fn replace(&mut self, _: &Self::Match, _: &SearchQuery, _: &mut ViewContext<Self>) {
        // Since LSP Log is read-only, it doesn't make sense to support replace operation.
    }
    fn supported_options() -> workspace::searchable::SearchOptions {
        workspace::searchable::SearchOptions {
            case: true,
            word: true,
            regex: true,
            // LSP log is read-only.
            replacement: false,
        }
    }
    fn active_match_index(
        &mut self,
        matches: Vec<Self::Match>,
        cx: &mut ViewContext<Self>,
    ) -> Option<usize> {
        self.editor
            .update(cx, |e, cx| e.active_match_index(matches, cx))
    }
}

impl ToolbarItemView for LspLogToolbarItemView {
    fn set_active_pane_item(
        &mut self,
        active_pane_item: Option<&dyn ItemHandle>,
        cx: &mut ViewContext<Self>,
    ) -> workspace::ToolbarItemLocation {
        self.menu_open = false;
        if let Some(item) = active_pane_item {
            if let Some(log_view) = item.downcast::<LspLogView>() {
                self.log_view = Some(log_view.clone());
                self._log_view_subscription = Some(cx.observe(&log_view, |_, _, cx| {
                    cx.notify();
                }));
                return ToolbarItemLocation::PrimaryLeft {
                    flex: Some((1., false)),
                };
            }
        }
        self.log_view = None;
        self._log_view_subscription = None;
        ToolbarItemLocation::Hidden
    }
}

impl View for LspLogToolbarItemView {
    fn ui_name() -> &'static str {
        "LspLogView"
    }

    fn render(&mut self, cx: &mut ViewContext<Self>) -> AnyElement<Self> {
        let theme = theme::current(cx).clone();
        let Some(log_view) = self.log_view.as_ref() else {
            return Empty::new().into_any();
        };
        let (menu_rows, current_server_id) = log_view.update(cx, |log_view, cx| {
            let menu_rows = log_view.menu_items(cx).unwrap_or_default();
            let current_server_id = log_view.current_server_id;
            (menu_rows, current_server_id)
        });

        let current_server = current_server_id.and_then(|current_server_id| {
            if let Ok(ix) = menu_rows.binary_search_by_key(&current_server_id, |e| e.server_id) {
                Some(menu_rows[ix].clone())
            } else {
                None
            }
        });
        let server_selected = current_server.is_some();

        enum Menu {}
        let lsp_menu = Stack::new()
            .with_child(Self::render_language_server_menu_header(
                current_server,
                &theme,
                cx,
            ))
            .with_children(if self.menu_open {
                Some(
                    Overlay::new(
                        MouseEventHandler::new::<Menu, _>(0, cx, move |_, cx| {
                            Flex::column()
                                .scrollable::<Self>(0, None, cx)
                                .with_children(menu_rows.into_iter().map(|row| {
                                    Self::render_language_server_menu_item(
                                        row.server_id,
                                        row.server_name,
                                        &row.worktree_root_name,
                                        row.rpc_trace_enabled,
                                        row.logs_selected,
                                        row.rpc_trace_selected,
                                        &theme,
                                        cx,
                                    )
                                }))
                                .contained()
                                .with_style(theme.toolbar_dropdown_menu.container)
                                .constrained()
                                .with_width(400.)
                                .with_height(400.)
                        })
                        .on_down_out(MouseButton::Left, |_, this, cx| {
                            this.menu_open = false;
                            cx.notify()
                        }),
                    )
                    .with_hoverable(true)
                    .with_fit_mode(OverlayFitMode::SwitchAnchor)
                    .with_anchor_corner(AnchorCorner::TopLeft)
                    .with_z_index(999)
                    .aligned()
                    .bottom()
                    .left(),
                )
            } else {
                None
            })
            .aligned()
            .left()
            .clipped();

        enum LspCleanupButton {}
        let log_cleanup_button =
            MouseEventHandler::new::<LspCleanupButton, _>(1, cx, |state, cx| {
                let theme = theme::current(cx).clone();
                let style = theme
                    .workspace
                    .toolbar
                    .toggleable_text_tool
                    .in_state(server_selected)
                    .style_for(state);
                Label::new("Clear", style.text.clone())
                    .aligned()
                    .contained()
                    .with_style(style.container)
                    .constrained()
                    .with_height(theme.toolbar_dropdown_menu.row_height / 6.0 * 5.0)
            })
            .on_click(MouseButton::Left, move |_, this, cx| {
                if let Some(log_view) = this.log_view.as_ref() {
                    log_view.update(cx, |log_view, cx| {
                        log_view.editor.update(cx, |editor, cx| {
                            editor.set_read_only(false);
                            editor.clear(cx);
                            editor.set_read_only(true);
                        });
                    })
                }
            })
            .with_cursor_style(CursorStyle::PointingHand)
            .aligned()
            .right();

        Flex::row()
            .with_child(lsp_menu)
            .with_child(log_cleanup_button)
            .contained()
            .aligned()
            .left()
            .into_any_named("lsp log controls")
    }
}

const RPC_MESSAGES: &str = "RPC Messages";
const SERVER_LOGS: &str = "Server Logs";

impl LspLogToolbarItemView {
    pub fn new() -> Self {
        Self {
            menu_open: false,
            log_view: None,
            _log_view_subscription: None,
        }
    }

    fn toggle_menu(&mut self, cx: &mut ViewContext<Self>) {
        self.menu_open = !self.menu_open;
        cx.notify();
    }

    fn toggle_logging_for_server(
        &mut self,
        id: LanguageServerId,
        enabled: bool,
        cx: &mut ViewContext<Self>,
    ) {
        if let Some(log_view) = &self.log_view {
            log_view.update(cx, |log_view, cx| {
                log_view.toggle_rpc_trace_for_server(id, enabled, cx);
                if !enabled && Some(id) == log_view.current_server_id {
                    log_view.show_logs_for_server(id, cx);
                    cx.notify();
                }
            });
        }
        cx.notify();
    }

    fn show_logs_for_server(&mut self, id: LanguageServerId, cx: &mut ViewContext<Self>) {
        if let Some(log_view) = &self.log_view {
            log_view.update(cx, |view, cx| view.show_logs_for_server(id, cx));
            self.menu_open = false;
            cx.notify();
        }
    }

    fn show_rpc_trace_for_server(&mut self, id: LanguageServerId, cx: &mut ViewContext<Self>) {
        if let Some(log_view) = &self.log_view {
            log_view.update(cx, |view, cx| view.show_rpc_trace_for_server(id, cx));
            self.menu_open = false;
            cx.notify();
        }
    }

    fn render_language_server_menu_header(
        current_server: Option<LogMenuItem>,
        theme: &Arc<Theme>,
        cx: &mut ViewContext<Self>,
    ) -> impl Element<Self> {
        enum ToggleMenu {}
        MouseEventHandler::new::<ToggleMenu, _>(0, cx, move |state, _| {
            let label: Cow<str> = current_server
                .and_then(|row| {
                    Some(
                        format!(
                            "{} ({}) - {}",
                            row.server_name.0,
                            row.worktree_root_name,
                            if row.rpc_trace_selected {
                                RPC_MESSAGES
                            } else {
                                SERVER_LOGS
                            },
                        )
                        .into(),
                    )
                })
                .unwrap_or_else(|| "No server selected".into());
            let style = theme.toolbar_dropdown_menu.header.style_for(state);
            Label::new(label, style.text.clone())
                .contained()
                .with_style(style.container)
        })
        .with_cursor_style(CursorStyle::PointingHand)
        .on_click(MouseButton::Left, move |_, view, cx| {
            view.toggle_menu(cx);
        })
    }

    fn render_language_server_menu_item(
        id: LanguageServerId,
        name: LanguageServerName,
        worktree_root_name: &str,
        rpc_trace_enabled: bool,
        logs_selected: bool,
        rpc_trace_selected: bool,
        theme: &Arc<Theme>,
        cx: &mut ViewContext<Self>,
    ) -> impl Element<Self> {
        enum ActivateLog {}
        enum ActivateRpcTrace {}

        Flex::column()
            .with_child({
                let style = &theme.toolbar_dropdown_menu.section_header;
                Label::new(
                    format!("{} ({})", name.0, worktree_root_name),
                    style.text.clone(),
                )
                .contained()
                .with_style(style.container)
                .constrained()
                .with_height(theme.toolbar_dropdown_menu.row_height)
            })
            .with_child(
                MouseEventHandler::new::<ActivateLog, _>(id.0, cx, move |state, _| {
                    let style = theme
                        .toolbar_dropdown_menu
                        .item
                        .in_state(logs_selected)
                        .style_for(state);
                    Label::new(SERVER_LOGS, style.text.clone())
                        .contained()
                        .with_style(style.container)
                        .constrained()
                        .with_height(theme.toolbar_dropdown_menu.row_height)
                })
                .with_cursor_style(CursorStyle::PointingHand)
                .on_click(MouseButton::Left, move |_, view, cx| {
                    view.show_logs_for_server(id, cx);
                }),
            )
            .with_child(
                MouseEventHandler::new::<ActivateRpcTrace, _>(id.0, cx, move |state, cx| {
                    let style = theme
                        .toolbar_dropdown_menu
                        .item
                        .in_state(rpc_trace_selected)
                        .style_for(state);
                    Flex::row()
                        .with_child(
                            Label::new(RPC_MESSAGES, style.text.clone())
                                .constrained()
                                .with_height(theme.toolbar_dropdown_menu.row_height),
                        )
                        .with_child(
                            ui::checkbox_with_label::<Self, _, Self, _>(
                                Empty::new(),
                                &theme.welcome.checkbox,
                                rpc_trace_enabled,
                                id.0,
                                cx,
                                move |this, enabled, cx| {
                                    this.toggle_logging_for_server(id, enabled, cx);
                                },
                            )
                            .flex_float(),
                        )
                        .align_children_center()
                        .contained()
                        .with_style(style.container)
                        .constrained()
                        .with_height(theme.toolbar_dropdown_menu.row_height)
                })
                .with_cursor_style(CursorStyle::PointingHand)
                .on_click(MouseButton::Left, move |_, view, cx| {
                    view.show_rpc_trace_for_server(id, cx);
                }),
            )
    }
}

pub enum Event {
    NewServerLogEntry {
        id: LanguageServerId,
        entry: String,
        is_rpc: bool,
    },
}

impl Entity for LogStore {
    type Event = Event;
}

impl Entity for LspLogView {
    type Event = editor::Event;
}

impl Entity for LspLogToolbarItemView {
    type Event = ();
}
