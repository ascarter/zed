use crate::{
    display_map::{invisibles::is_invisible, InlayOffset, ToDisplayPoint},
    hover_links::{InlayHighlight, RangeInEditor},
    scroll::ScrollAmount,
    Anchor, AnchorRangeExt, DisplayPoint, DisplayRow, Editor, EditorSettings, EditorSnapshot,
    Hover,
};
use gpui::{
    div, px, AnyElement, AsyncWindowContext, Context, Entity, Focusable as _, FontWeight, Hsla,
    InteractiveElement, IntoElement, MouseButton, ParentElement, Pixels, ScrollHandle, Size,
    Stateful, StatefulInteractiveElement, StyleRefinement, Styled, Task, TextStyleRefinement,
    Window,
};
use itertools::Itertools;
use language::{DiagnosticEntry, Language, LanguageRegistry};
use lsp::DiagnosticSeverity;
use markdown::{Markdown, MarkdownStyle};
use multi_buffer::ToOffset;
use project::{HoverBlock, HoverBlockKind, InlayHintLabelPart};
use settings::Settings;
use std::rc::Rc;
use std::{borrow::Cow, cell::RefCell};
use std::{ops::Range, sync::Arc, time::Duration};
use theme::ThemeSettings;
use ui::{prelude::*, theme_is_transparent, Scrollbar, ScrollbarState};
use util::TryFutureExt;
pub const HOVER_REQUEST_DELAY_MILLIS: u64 = 200;

pub const MIN_POPOVER_CHARACTER_WIDTH: f32 = 20.;
pub const MIN_POPOVER_LINE_HEIGHT: Pixels = px(4.);
pub const HOVER_POPOVER_GAP: Pixels = px(10.);

/// Bindable action which uses the most recent selection head to trigger a hover
pub fn hover(editor: &mut Editor, _: &Hover, window: &mut Window, cx: &mut Context<Editor>) {
    let head = editor.selections.newest_anchor().head();
    show_hover(editor, head, true, window, cx);
}

/// The internal hover action dispatches between `show_hover` or `hide_hover`
/// depending on whether a point to hover over is provided.
pub fn hover_at(
    editor: &mut Editor,
    anchor: Option<Anchor>,
    window: &mut Window,
    cx: &mut Context<Editor>,
) {
    if EditorSettings::get_global(cx).hover_popover_enabled {
        if show_keyboard_hover(editor, window, cx) {
            return;
        }
        if let Some(anchor) = anchor {
            show_hover(editor, anchor, false, window, cx);
        } else {
            hide_hover(editor, cx);
        }
    }
}

pub fn show_keyboard_hover(
    editor: &mut Editor,
    window: &mut Window,
    cx: &mut Context<Editor>,
) -> bool {
    let info_popovers = editor.hover_state.info_popovers.clone();
    for p in info_popovers {
        let keyboard_grace = p.keyboard_grace.borrow();
        if *keyboard_grace {
            if let Some(anchor) = p.anchor {
                show_hover(editor, anchor, false, window, cx);
                return true;
            }
        }
    }

    let diagnostic_popover = editor.hover_state.diagnostic_popover.clone();
    if let Some(d) = diagnostic_popover {
        let keyboard_grace = d.keyboard_grace.borrow();
        if *keyboard_grace {
            if let Some(anchor) = d.anchor {
                show_hover(editor, anchor, false, window, cx);
                return true;
            }
        }
    }

    false
}

pub struct InlayHover {
    pub range: InlayHighlight,
    pub tooltip: HoverBlock,
}

pub fn find_hovered_hint_part(
    label_parts: Vec<InlayHintLabelPart>,
    hint_start: InlayOffset,
    hovered_offset: InlayOffset,
) -> Option<(InlayHintLabelPart, Range<InlayOffset>)> {
    if hovered_offset >= hint_start {
        let mut hovered_character = (hovered_offset - hint_start).0;
        let mut part_start = hint_start;
        for part in label_parts {
            let part_len = part.value.chars().count();
            if hovered_character > part_len {
                hovered_character -= part_len;
                part_start.0 += part_len;
            } else {
                let part_end = InlayOffset(part_start.0 + part_len);
                return Some((part, part_start..part_end));
            }
        }
    }
    None
}

pub fn hover_at_inlay(
    editor: &mut Editor,
    inlay_hover: InlayHover,
    window: &mut Window,
    cx: &mut Context<Editor>,
) {
    if EditorSettings::get_global(cx).hover_popover_enabled {
        if editor.pending_rename.is_some() {
            return;
        }

        let Some(project) = editor.project.clone() else {
            return;
        };

        if editor
            .hover_state
            .info_popovers
            .iter()
            .any(|InfoPopover { symbol_range, .. }| {
                if let RangeInEditor::Inlay(range) = symbol_range {
                    if range == &inlay_hover.range {
                        // Hover triggered from same location as last time. Don't show again.
                        return true;
                    }
                }
                false
            })
        {
            hide_hover(editor, cx);
        }

        let hover_popover_delay = EditorSettings::get_global(cx).hover_popover_delay;

        let task = cx.spawn_in(window, |this, mut cx| {
            async move {
                cx.background_executor()
                    .timer(Duration::from_millis(hover_popover_delay))
                    .await;
                this.update(&mut cx, |this, _| {
                    this.hover_state.diagnostic_popover = None;
                })?;

                let language_registry = project.update(&mut cx, |p, _| p.languages().clone())?;
                let blocks = vec![inlay_hover.tooltip];
                let parsed_content = parse_blocks(&blocks, &language_registry, None, &mut cx).await;

                let scroll_handle = ScrollHandle::new();
                let hover_popover = InfoPopover {
                    symbol_range: RangeInEditor::Inlay(inlay_hover.range.clone()),
                    parsed_content,
                    scrollbar_state: ScrollbarState::new(scroll_handle.clone()),
                    scroll_handle,
                    keyboard_grace: Rc::new(RefCell::new(false)),
                    anchor: None,
                };

                this.update(&mut cx, |this, cx| {
                    // TODO: no background highlights happen for inlays currently
                    this.hover_state.info_popovers = vec![hover_popover];
                    cx.notify();
                })?;

                anyhow::Ok(())
            }
            .log_err()
        });

        editor.hover_state.info_task = Some(task);
    }
}

/// Hides the type information popup.
/// Triggered by the `Hover` action when the cursor is not over a symbol or when the
/// selections changed.
pub fn hide_hover(editor: &mut Editor, cx: &mut Context<Editor>) -> bool {
    let info_popovers = editor.hover_state.info_popovers.drain(..);
    let diagnostics_popover = editor.hover_state.diagnostic_popover.take();
    let did_hide = info_popovers.count() > 0 || diagnostics_popover.is_some();

    editor.hover_state.info_task = None;
    editor.hover_state.triggered_from = None;

    editor.clear_background_highlights::<HoverState>(cx);

    if did_hide {
        cx.notify();
    }

    did_hide
}

/// Queries the LSP and shows type info and documentation
/// about the symbol the mouse is currently hovering over.
/// Triggered by the `Hover` action when the cursor may be over a symbol.
fn show_hover(
    editor: &mut Editor,
    anchor: Anchor,
    ignore_timeout: bool,
    window: &mut Window,
    cx: &mut Context<Editor>,
) -> Option<()> {
    if editor.pending_rename.is_some() {
        return None;
    }

    let snapshot = editor.snapshot(window, cx);

    let (buffer, buffer_position) = editor
        .buffer
        .read(cx)
        .text_anchor_for_position(anchor, cx)?;

    let (excerpt_id, _, _) = editor.buffer().read(cx).excerpt_containing(anchor, cx)?;

    let language_registry = editor.project.as_ref()?.read(cx).languages().clone();
    let provider = editor.semantics_provider.clone()?;

    if !ignore_timeout {
        if same_info_hover(editor, &snapshot, anchor)
            || same_diagnostic_hover(editor, &snapshot, anchor)
            || editor.hover_state.diagnostic_popover.is_some()
        {
            // Hover triggered from same location as last time. Don't show again.
            return None;
        } else {
            hide_hover(editor, cx);
        }
    }

    // Don't request again if the location is the same as the previous request
    if let Some(triggered_from) = &editor.hover_state.triggered_from {
        if triggered_from
            .cmp(&anchor, &snapshot.buffer_snapshot)
            .is_eq()
        {
            return None;
        }
    }

    let hover_popover_delay = EditorSettings::get_global(cx).hover_popover_delay;

    let task = cx.spawn_in(window, |this, mut cx| {
        async move {
            // If we need to delay, delay a set amount initially before making the lsp request
            let delay = if ignore_timeout {
                None
            } else {
                // Construct delay task to wait for later
                let total_delay = Some(
                    cx.background_executor()
                        .timer(Duration::from_millis(hover_popover_delay)),
                );

                cx.background_executor()
                    .timer(Duration::from_millis(HOVER_REQUEST_DELAY_MILLIS))
                    .await;
                total_delay
            };

            let hover_request = cx.update(|_, cx| provider.hover(&buffer, buffer_position, cx))?;

            if let Some(delay) = delay {
                delay.await;
            }

            let offset = anchor.to_offset(&snapshot.buffer_snapshot);
            let local_diagnostic = snapshot
                .buffer_snapshot
                .diagnostics_in_range::<usize>(offset..offset)
                // Find the entry with the most specific range
                .min_by_key(|entry| entry.range.len());

            let diagnostic_popover = if let Some(local_diagnostic) = local_diagnostic {
                let text = match local_diagnostic.diagnostic.source {
                    Some(ref source) => {
                        format!("{source}: {}", local_diagnostic.diagnostic.message)
                    }
                    None => local_diagnostic.diagnostic.message.clone(),
                };
                let local_diagnostic = DiagnosticEntry {
                    diagnostic: local_diagnostic.diagnostic,
                    range: snapshot
                        .buffer_snapshot
                        .anchor_before(local_diagnostic.range.start)
                        ..snapshot
                            .buffer_snapshot
                            .anchor_after(local_diagnostic.range.end),
                };

                let mut border_color: Option<Hsla> = None;
                let mut background_color: Option<Hsla> = None;

                let parsed_content = cx
                    .new_window_entity(|window, cx| {
                        let status_colors = cx.theme().status();

                        match local_diagnostic.diagnostic.severity {
                            DiagnosticSeverity::ERROR => {
                                background_color = Some(status_colors.error_background);
                                border_color = Some(status_colors.error_border);
                            }
                            DiagnosticSeverity::WARNING => {
                                background_color = Some(status_colors.warning_background);
                                border_color = Some(status_colors.warning_border);
                            }
                            DiagnosticSeverity::INFORMATION => {
                                background_color = Some(status_colors.info_background);
                                border_color = Some(status_colors.info_border);
                            }
                            DiagnosticSeverity::HINT => {
                                background_color = Some(status_colors.hint_background);
                                border_color = Some(status_colors.hint_border);
                            }
                            _ => {
                                background_color = Some(status_colors.ignored_background);
                                border_color = Some(status_colors.ignored_border);
                            }
                        };
                        let settings = ThemeSettings::get_global(cx);
                        let mut base_text_style = window.text_style();
                        base_text_style.refine(&TextStyleRefinement {
                            font_family: Some(settings.ui_font.family.clone()),
                            font_fallbacks: settings.ui_font.fallbacks.clone(),
                            font_size: Some(settings.ui_font_size.into()),
                            color: Some(cx.theme().colors().editor_foreground),
                            background_color: Some(gpui::transparent_black()),

                            ..Default::default()
                        });
                        let markdown_style = MarkdownStyle {
                            base_text_style,
                            selection_background_color: { cx.theme().players().local().selection },
                            link: TextStyleRefinement {
                                underline: Some(gpui::UnderlineStyle {
                                    thickness: px(1.),
                                    color: Some(cx.theme().colors().editor_foreground),
                                    wavy: false,
                                }),
                                ..Default::default()
                            },
                            ..Default::default()
                        };
                        Markdown::new_text(text, markdown_style.clone(), None, None, window, cx)
                    })
                    .ok();

                Some(DiagnosticPopover {
                    local_diagnostic,
                    parsed_content,
                    border_color,
                    background_color,
                    keyboard_grace: Rc::new(RefCell::new(ignore_timeout)),
                    anchor: Some(anchor),
                })
            } else {
                None
            };

            this.update(&mut cx, |this, _| {
                this.hover_state.diagnostic_popover = diagnostic_popover;
            })?;

            let invisible_char = if let Some(invisible) = snapshot
                .buffer_snapshot
                .chars_at(anchor)
                .next()
                .filter(|&c| is_invisible(c))
            {
                let after = snapshot.buffer_snapshot.anchor_after(
                    anchor.to_offset(&snapshot.buffer_snapshot) + invisible.len_utf8(),
                );
                Some((invisible, anchor..after))
            } else if let Some(invisible) = snapshot
                .buffer_snapshot
                .reversed_chars_at(anchor)
                .next()
                .filter(|&c| is_invisible(c))
            {
                let before = snapshot.buffer_snapshot.anchor_before(
                    anchor.to_offset(&snapshot.buffer_snapshot) - invisible.len_utf8(),
                );

                Some((invisible, before..anchor))
            } else {
                None
            };

            let hovers_response = if let Some(hover_request) = hover_request {
                hover_request.await
            } else {
                Vec::new()
            };
            let snapshot = this.update_in(&mut cx, |this, window, cx| this.snapshot(window, cx))?;
            let mut hover_highlights = Vec::with_capacity(hovers_response.len());
            let mut info_popovers = Vec::with_capacity(
                hovers_response.len() + if invisible_char.is_some() { 1 } else { 0 },
            );

            if let Some((invisible, range)) = invisible_char {
                let blocks = vec![HoverBlock {
                    text: format!("Unicode character U+{:02X}", invisible as u32),
                    kind: HoverBlockKind::PlainText,
                }];
                let parsed_content = parse_blocks(&blocks, &language_registry, None, &mut cx).await;
                let scroll_handle = ScrollHandle::new();
                info_popovers.push(InfoPopover {
                    symbol_range: RangeInEditor::Text(range),
                    parsed_content,
                    scrollbar_state: ScrollbarState::new(scroll_handle.clone()),
                    scroll_handle,
                    keyboard_grace: Rc::new(RefCell::new(ignore_timeout)),
                    anchor: Some(anchor),
                })
            }

            for hover_result in hovers_response {
                // Create symbol range of anchors for highlighting and filtering of future requests.
                let range = hover_result
                    .range
                    .and_then(|range| {
                        let start = snapshot
                            .buffer_snapshot
                            .anchor_in_excerpt(excerpt_id, range.start)?;
                        let end = snapshot
                            .buffer_snapshot
                            .anchor_in_excerpt(excerpt_id, range.end)?;
                        Some(start..end)
                    })
                    .or_else(|| {
                        let snapshot = &snapshot.buffer_snapshot;
                        let offset_range = snapshot.syntax_ancestor(anchor..anchor)?.1;
                        Some(
                            snapshot.anchor_before(offset_range.start)
                                ..snapshot.anchor_after(offset_range.end),
                        )
                    })
                    .unwrap_or_else(|| anchor..anchor);

                let blocks = hover_result.contents;
                let language = hover_result.language;
                let parsed_content =
                    parse_blocks(&blocks, &language_registry, language, &mut cx).await;
                let scroll_handle = ScrollHandle::new();
                hover_highlights.push(range.clone());
                info_popovers.push(InfoPopover {
                    symbol_range: RangeInEditor::Text(range),
                    parsed_content,
                    scrollbar_state: ScrollbarState::new(scroll_handle.clone()),
                    scroll_handle,
                    keyboard_grace: Rc::new(RefCell::new(ignore_timeout)),
                    anchor: Some(anchor),
                });
            }

            this.update_in(&mut cx, |editor, window, cx| {
                if hover_highlights.is_empty() {
                    editor.clear_background_highlights::<HoverState>(cx);
                } else {
                    // Highlight the selected symbol using a background highlight
                    editor.highlight_background::<HoverState>(
                        &hover_highlights,
                        |theme| theme.element_hover, // todo update theme
                        cx,
                    );
                }

                editor.hover_state.info_popovers = info_popovers;
                cx.notify();
                window.refresh();
            })?;

            anyhow::Ok(())
        }
        .log_err()
    });

    editor.hover_state.info_task = Some(task);
    None
}

fn same_info_hover(editor: &Editor, snapshot: &EditorSnapshot, anchor: Anchor) -> bool {
    editor
        .hover_state
        .info_popovers
        .iter()
        .any(|InfoPopover { symbol_range, .. }| {
            symbol_range
                .as_text_range()
                .map(|range| {
                    let hover_range = range.to_offset(&snapshot.buffer_snapshot);
                    let offset = anchor.to_offset(&snapshot.buffer_snapshot);
                    // LSP returns a hover result for the end index of ranges that should be hovered, so we need to
                    // use an inclusive range here to check if we should dismiss the popover
                    (hover_range.start..=hover_range.end).contains(&offset)
                })
                .unwrap_or(false)
        })
}

fn same_diagnostic_hover(editor: &Editor, snapshot: &EditorSnapshot, anchor: Anchor) -> bool {
    editor
        .hover_state
        .diagnostic_popover
        .as_ref()
        .map(|diagnostic| {
            let hover_range = diagnostic
                .local_diagnostic
                .range
                .to_offset(&snapshot.buffer_snapshot);
            let offset = anchor.to_offset(&snapshot.buffer_snapshot);

            // Here we do basically the same as in `same_info_hover`, see comment there for an explanation
            (hover_range.start..=hover_range.end).contains(&offset)
        })
        .unwrap_or(false)
}

async fn parse_blocks(
    blocks: &[HoverBlock],
    language_registry: &Arc<LanguageRegistry>,
    language: Option<Arc<Language>>,
    cx: &mut AsyncWindowContext,
) -> Option<Entity<Markdown>> {
    let fallback_language_name = if let Some(ref l) = language {
        let l = Arc::clone(l);
        Some(l.lsp_id().clone())
    } else {
        None
    };

    let combined_text = blocks
        .iter()
        .map(|block| match &block.kind {
            project::HoverBlockKind::PlainText | project::HoverBlockKind::Markdown => {
                Cow::Borrowed(block.text.trim())
            }
            project::HoverBlockKind::Code { language } => {
                Cow::Owned(format!("```{}\n{}\n```", language, block.text.trim()))
            }
        })
        .join("\n\n");

    let rendered_block = cx
        .new_window_entity(|window, cx| {
            let settings = ThemeSettings::get_global(cx);
            let ui_font_family = settings.ui_font.family.clone();
            let ui_font_fallbacks = settings.ui_font.fallbacks.clone();
            let buffer_font_family = settings.buffer_font.family.clone();
            let buffer_font_fallbacks = settings.buffer_font.fallbacks.clone();

            let mut base_text_style = window.text_style();
            base_text_style.refine(&TextStyleRefinement {
                font_family: Some(ui_font_family.clone()),
                font_fallbacks: ui_font_fallbacks,
                color: Some(cx.theme().colors().editor_foreground),
                ..Default::default()
            });

            let markdown_style = MarkdownStyle {
                base_text_style,
                code_block: StyleRefinement::default().my(rems(1.)).font_buffer(cx),
                inline_code: TextStyleRefinement {
                    background_color: Some(cx.theme().colors().background),
                    font_family: Some(buffer_font_family),
                    font_fallbacks: buffer_font_fallbacks,
                    ..Default::default()
                },
                rule_color: cx.theme().colors().border,
                block_quote_border_color: Color::Muted.color(cx),
                block_quote: TextStyleRefinement {
                    color: Some(Color::Muted.color(cx)),
                    ..Default::default()
                },
                link: TextStyleRefinement {
                    color: Some(cx.theme().colors().editor_foreground),
                    underline: Some(gpui::UnderlineStyle {
                        thickness: px(1.),
                        color: Some(cx.theme().colors().editor_foreground),
                        wavy: false,
                    }),
                    ..Default::default()
                },
                syntax: cx.theme().syntax().clone(),
                selection_background_color: { cx.theme().players().local().selection },
                break_style: Default::default(),
                heading: StyleRefinement::default()
                    .font_weight(FontWeight::BOLD)
                    .text_base()
                    .mt(rems(1.))
                    .mb_0(),
            };

            Markdown::new(
                combined_text,
                markdown_style.clone(),
                Some(language_registry.clone()),
                fallback_language_name,
                window,
                cx,
            )
            .copy_code_block_buttons(false)
        })
        .ok();

    rendered_block
}

#[derive(Default, Debug)]
pub struct HoverState {
    pub info_popovers: Vec<InfoPopover>,
    pub diagnostic_popover: Option<DiagnosticPopover>,
    pub triggered_from: Option<Anchor>,
    pub info_task: Option<Task<Option<()>>>,
}

impl HoverState {
    pub fn visible(&self) -> bool {
        !self.info_popovers.is_empty() || self.diagnostic_popover.is_some()
    }

    pub(crate) fn render(
        &mut self,
        snapshot: &EditorSnapshot,
        visible_rows: Range<DisplayRow>,
        max_size: Size<Pixels>,
        cx: &mut Context<Editor>,
    ) -> Option<(DisplayPoint, Vec<AnyElement>)> {
        // If there is a diagnostic, position the popovers based on that.
        // Otherwise use the start of the hover range
        let anchor = self
            .diagnostic_popover
            .as_ref()
            .map(|diagnostic_popover| &diagnostic_popover.local_diagnostic.range.start)
            .or_else(|| {
                self.info_popovers.iter().find_map(|info_popover| {
                    match &info_popover.symbol_range {
                        RangeInEditor::Text(range) => Some(&range.start),
                        RangeInEditor::Inlay(_) => None,
                    }
                })
            })
            .or_else(|| {
                self.info_popovers.iter().find_map(|info_popover| {
                    match &info_popover.symbol_range {
                        RangeInEditor::Text(_) => None,
                        RangeInEditor::Inlay(range) => Some(&range.inlay_position),
                    }
                })
            })?;
        let point = anchor.to_display_point(&snapshot.display_snapshot);

        // Don't render if the relevant point isn't on screen
        if !self.visible() || !visible_rows.contains(&point.row()) {
            return None;
        }

        let mut elements = Vec::new();

        if let Some(diagnostic_popover) = self.diagnostic_popover.as_ref() {
            elements.push(diagnostic_popover.render(max_size, cx));
        }
        for info_popover in &mut self.info_popovers {
            elements.push(info_popover.render(max_size, cx));
        }

        Some((point, elements))
    }

    pub fn focused(&self, window: &mut Window, cx: &mut Context<Editor>) -> bool {
        let mut hover_popover_is_focused = false;
        for info_popover in &self.info_popovers {
            if let Some(markdown_view) = &info_popover.parsed_content {
                if markdown_view.focus_handle(cx).is_focused(window) {
                    hover_popover_is_focused = true;
                }
            }
        }
        if let Some(diagnostic_popover) = &self.diagnostic_popover {
            if let Some(markdown_view) = &diagnostic_popover.parsed_content {
                if markdown_view.focus_handle(cx).is_focused(window) {
                    hover_popover_is_focused = true;
                }
            }
        }
        hover_popover_is_focused
    }
}

#[derive(Debug, Clone)]
pub(crate) struct InfoPopover {
    pub(crate) symbol_range: RangeInEditor,
    pub(crate) parsed_content: Option<Entity<Markdown>>,
    pub(crate) scroll_handle: ScrollHandle,
    pub(crate) scrollbar_state: ScrollbarState,
    pub(crate) keyboard_grace: Rc<RefCell<bool>>,
    pub(crate) anchor: Option<Anchor>,
}

impl InfoPopover {
    pub(crate) fn render(
        &mut self,
        max_size: Size<Pixels>,
        cx: &mut Context<Editor>,
    ) -> AnyElement {
        let keyboard_grace = Rc::clone(&self.keyboard_grace);
        let mut d = div()
            .id("info_popover")
            .elevation_2(cx)
            // Prevent a mouse down/move on the popover from being propagated to the editor,
            // because that would dismiss the popover.
            .on_mouse_move(|_, _, cx| cx.stop_propagation())
            .on_mouse_down(MouseButton::Left, move |_, _, cx| {
                let mut keyboard_grace = keyboard_grace.borrow_mut();
                *keyboard_grace = false;
                cx.stop_propagation();
            });

        if let Some(markdown) = &self.parsed_content {
            d = d
                .child(
                    div()
                        .id("info-md-container")
                        .overflow_y_scroll()
                        .max_w(max_size.width)
                        .max_h(max_size.height)
                        .p_2()
                        .track_scroll(&self.scroll_handle)
                        .child(markdown.clone()),
                )
                .child(self.render_vertical_scrollbar(cx));
        }
        d.into_any_element()
    }

    pub fn scroll(&self, amount: &ScrollAmount, window: &mut Window, cx: &mut Context<Editor>) {
        let mut current = self.scroll_handle.offset();
        current.y -= amount.pixels(
            window.line_height(),
            self.scroll_handle.bounds().size.height - px(16.),
        ) / 2.0;
        cx.notify();
        self.scroll_handle.set_offset(current);
    }

    fn render_vertical_scrollbar(&self, cx: &mut Context<Editor>) -> Stateful<Div> {
        div()
            .occlude()
            .id("info-popover-vertical-scroll")
            .on_mouse_move(cx.listener(|_, _, _, cx| {
                cx.notify();
                cx.stop_propagation()
            }))
            .on_hover(|_, _, cx| {
                cx.stop_propagation();
            })
            .on_any_mouse_down(|_, _, cx| {
                cx.stop_propagation();
            })
            .on_mouse_up(
                MouseButton::Left,
                cx.listener(|_, _, _, cx| {
                    cx.stop_propagation();
                }),
            )
            .on_scroll_wheel(cx.listener(|_, _, _, cx| {
                cx.notify();
            }))
            .h_full()
            .absolute()
            .right_1()
            .top_1()
            .bottom_0()
            .w(px(12.))
            .cursor_default()
            .children(Scrollbar::vertical(self.scrollbar_state.clone()))
    }
}

#[derive(Debug, Clone)]
pub struct DiagnosticPopover {
    pub(crate) local_diagnostic: DiagnosticEntry<Anchor>,
    parsed_content: Option<Entity<Markdown>>,
    border_color: Option<Hsla>,
    background_color: Option<Hsla>,
    pub keyboard_grace: Rc<RefCell<bool>>,
    pub anchor: Option<Anchor>,
}

impl DiagnosticPopover {
    pub fn render(&self, max_size: Size<Pixels>, cx: &mut Context<Editor>) -> AnyElement {
        let keyboard_grace = Rc::clone(&self.keyboard_grace);
        let mut markdown_div = div().py_1().px_2();
        if let Some(markdown) = &self.parsed_content {
            markdown_div = markdown_div.child(markdown.clone());
        }

        if let Some(background_color) = &self.background_color {
            markdown_div = markdown_div.bg(*background_color);
        }

        if let Some(border_color) = &self.border_color {
            markdown_div = markdown_div
                .border_1()
                .border_color(*border_color)
                .rounded_lg();
        }

        let diagnostic_div = div()
            .id("diagnostic")
            .block()
            .max_h(max_size.height)
            .overflow_y_scroll()
            .max_w(max_size.width)
            .elevation_2_borderless(cx)
            // Don't draw the background color if the theme
            // allows transparent surfaces.
            .when(theme_is_transparent(cx), |this| {
                this.bg(gpui::transparent_black())
            })
            // Prevent a mouse move on the popover from being propagated to the editor,
            // because that would dismiss the popover.
            .on_mouse_move(|_, _, cx| cx.stop_propagation())
            // Prevent a mouse down on the popover from being propagated to the editor,
            // because that would move the cursor.
            .on_mouse_down(MouseButton::Left, move |_, _, cx| {
                let mut keyboard_grace = keyboard_grace.borrow_mut();
                *keyboard_grace = false;
                cx.stop_propagation();
            })
            .child(markdown_div);

        diagnostic_div.into_any_element()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        actions::ConfirmCompletion,
        editor_tests::{handle_completion_request, init_test},
        hover_links::update_inlay_link_and_hover_points,
        inlay_hint_cache::tests::{cached_hint_labels, visible_hint_labels},
        test::editor_lsp_test_context::EditorLspTestContext,
        InlayId, PointForPosition,
    };
    use collections::BTreeSet;
    use gpui::App;
    use indoc::indoc;
    use language::{language_settings::InlayHintSettings, Diagnostic, DiagnosticSet};
    use lsp::LanguageServerId;
    use markdown::parser::MarkdownEvent;
    use smol::stream::StreamExt;
    use std::sync::atomic;
    use std::sync::atomic::AtomicUsize;
    use text::Bias;

    fn get_hover_popover_delay(cx: &gpui::TestAppContext) -> u64 {
        cx.read(|cx: &App| -> u64 { EditorSettings::get_global(cx).hover_popover_delay })
    }

    impl InfoPopover {
        fn get_rendered_text(&self, cx: &gpui::App) -> String {
            let mut rendered_text = String::new();
            if let Some(parsed_content) = self.parsed_content.clone() {
                let markdown = parsed_content.read(cx);
                let text = markdown.parsed_markdown().source().to_string();
                let data = markdown.parsed_markdown().events();
                let slice = data;

                for (range, event) in slice.iter() {
                    if [MarkdownEvent::Text, MarkdownEvent::Code].contains(event) {
                        rendered_text.push_str(&text[range.clone()])
                    }
                }
            }
            rendered_text
        }
    }

    #[gpui::test]
    async fn test_mouse_hover_info_popover_with_autocomplete_popover(
        cx: &mut gpui::TestAppContext,
    ) {
        init_test(cx, |_| {});

        let mut cx = EditorLspTestContext::new_rust(
            lsp::ServerCapabilities {
                hover_provider: Some(lsp::HoverProviderCapability::Simple(true)),
                completion_provider: Some(lsp::CompletionOptions {
                    trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                    resolve_provider: Some(true),
                    ..Default::default()
                }),
                ..Default::default()
            },
            cx,
        )
        .await;
        let counter = Arc::new(AtomicUsize::new(0));
        // Basic hover delays and then pops without moving the mouse
        cx.set_state(indoc! {"
                oneˇ
                two
                three
                fn test() { println!(); }
            "});

        //prompt autocompletion menu
        cx.simulate_keystroke(".");
        handle_completion_request(
            &mut cx,
            indoc! {"
                        one.|<>
                        two
                        three
                    "},
            vec!["first_completion", "second_completion"],
            counter.clone(),
        )
        .await;
        cx.condition(|editor, _| editor.context_menu_visible()) // wait until completion menu is visible
            .await;
        assert_eq!(counter.load(atomic::Ordering::Acquire), 1); // 1 completion request

        let hover_point = cx.display_point(indoc! {"
                one.
                two
                three
                fn test() { printˇln!(); }
            "});
        cx.update_editor(|editor, window, cx| {
            let snapshot = editor.snapshot(window, cx);
            let anchor = snapshot
                .buffer_snapshot
                .anchor_before(hover_point.to_offset(&snapshot, Bias::Left));
            hover_at(editor, Some(anchor), window, cx)
        });
        assert!(!cx.editor(|editor, _window, _cx| editor.hover_state.visible()));

        // After delay, hover should be visible.
        let symbol_range = cx.lsp_range(indoc! {"
                one.
                two
                three
                fn test() { «println!»(); }
            "});
        let mut requests =
            cx.handle_request::<lsp::request::HoverRequest, _, _>(move |_, _, _| async move {
                Ok(Some(lsp::Hover {
                    contents: lsp::HoverContents::Markup(lsp::MarkupContent {
                        kind: lsp::MarkupKind::Markdown,
                        value: "some basic docs".to_string(),
                    }),
                    range: Some(symbol_range),
                }))
            });
        cx.background_executor
            .advance_clock(Duration::from_millis(get_hover_popover_delay(&cx) + 100));
        requests.next().await;

        cx.editor(|editor, _window, cx| {
            assert!(editor.hover_state.visible());
            assert_eq!(
                editor.hover_state.info_popovers.len(),
                1,
                "Expected exactly one hover but got: {:?}",
                editor.hover_state.info_popovers
            );
            let rendered_text = editor
                .hover_state
                .info_popovers
                .first()
                .unwrap()
                .get_rendered_text(cx);
            assert_eq!(rendered_text, "some basic docs".to_string())
        });

        // check that the completion menu is still visible and that there still has only been 1 completion request
        cx.editor(|editor, _, _| assert!(editor.context_menu_visible()));
        assert_eq!(counter.load(atomic::Ordering::Acquire), 1);

        //apply a completion and check it was successfully applied
        let _apply_additional_edits = cx.update_editor(|editor, window, cx| {
            editor.context_menu_next(&Default::default(), window, cx);
            editor
                .confirm_completion(&ConfirmCompletion::default(), window, cx)
                .unwrap()
        });
        cx.assert_editor_state(indoc! {"
            one.second_completionˇ
            two
            three
            fn test() { println!(); }
        "});

        // check that the completion menu is no longer visible and that there still has only been 1 completion request
        cx.editor(|editor, _, _| assert!(!editor.context_menu_visible()));
        assert_eq!(counter.load(atomic::Ordering::Acquire), 1);

        //verify the information popover is still visible and unchanged
        cx.editor(|editor, _, cx| {
            assert!(editor.hover_state.visible());
            assert_eq!(
                editor.hover_state.info_popovers.len(),
                1,
                "Expected exactly one hover but got: {:?}",
                editor.hover_state.info_popovers
            );
            let rendered_text = editor
                .hover_state
                .info_popovers
                .first()
                .unwrap()
                .get_rendered_text(cx);

            assert_eq!(rendered_text, "some basic docs".to_string())
        });

        // Mouse moved with no hover response dismisses
        let hover_point = cx.display_point(indoc! {"
                one.second_completionˇ
                two
                three
                fn teˇst() { println!(); }
            "});
        let mut request = cx
            .lsp
            .handle_request::<lsp::request::HoverRequest, _, _>(|_, _| async move { Ok(None) });
        cx.update_editor(|editor, window, cx| {
            let snapshot = editor.snapshot(window, cx);
            let anchor = snapshot
                .buffer_snapshot
                .anchor_before(hover_point.to_offset(&snapshot, Bias::Left));
            hover_at(editor, Some(anchor), window, cx)
        });
        cx.background_executor
            .advance_clock(Duration::from_millis(get_hover_popover_delay(&cx) + 100));
        request.next().await;

        // verify that the information popover is no longer visible
        cx.editor(|editor, _, _| {
            assert!(!editor.hover_state.visible());
        });
    }

    #[gpui::test]
    async fn test_mouse_hover_info_popover(cx: &mut gpui::TestAppContext) {
        init_test(cx, |_| {});

        let mut cx = EditorLspTestContext::new_rust(
            lsp::ServerCapabilities {
                hover_provider: Some(lsp::HoverProviderCapability::Simple(true)),
                ..Default::default()
            },
            cx,
        )
        .await;

        // Basic hover delays and then pops without moving the mouse
        cx.set_state(indoc! {"
            fn ˇtest() { println!(); }
        "});
        let hover_point = cx.display_point(indoc! {"
            fn test() { printˇln!(); }
        "});

        cx.update_editor(|editor, window, cx| {
            let snapshot = editor.snapshot(window, cx);
            let anchor = snapshot
                .buffer_snapshot
                .anchor_before(hover_point.to_offset(&snapshot, Bias::Left));
            hover_at(editor, Some(anchor), window, cx)
        });
        assert!(!cx.editor(|editor, _window, _cx| editor.hover_state.visible()));

        // After delay, hover should be visible.
        let symbol_range = cx.lsp_range(indoc! {"
            fn test() { «println!»(); }
        "});
        let mut requests =
            cx.handle_request::<lsp::request::HoverRequest, _, _>(move |_, _, _| async move {
                Ok(Some(lsp::Hover {
                    contents: lsp::HoverContents::Markup(lsp::MarkupContent {
                        kind: lsp::MarkupKind::Markdown,
                        value: "some basic docs".to_string(),
                    }),
                    range: Some(symbol_range),
                }))
            });
        cx.background_executor
            .advance_clock(Duration::from_millis(get_hover_popover_delay(&cx) + 100));
        requests.next().await;

        cx.editor(|editor, _, cx| {
            assert!(editor.hover_state.visible());
            assert_eq!(
                editor.hover_state.info_popovers.len(),
                1,
                "Expected exactly one hover but got: {:?}",
                editor.hover_state.info_popovers
            );
            let rendered_text = editor
                .hover_state
                .info_popovers
                .first()
                .unwrap()
                .get_rendered_text(cx);

            assert_eq!(rendered_text, "some basic docs".to_string())
        });

        // Mouse moved with no hover response dismisses
        let hover_point = cx.display_point(indoc! {"
            fn teˇst() { println!(); }
        "});
        let mut request = cx
            .lsp
            .handle_request::<lsp::request::HoverRequest, _, _>(|_, _| async move { Ok(None) });
        cx.update_editor(|editor, window, cx| {
            let snapshot = editor.snapshot(window, cx);
            let anchor = snapshot
                .buffer_snapshot
                .anchor_before(hover_point.to_offset(&snapshot, Bias::Left));
            hover_at(editor, Some(anchor), window, cx)
        });
        cx.background_executor
            .advance_clock(Duration::from_millis(get_hover_popover_delay(&cx) + 100));
        request.next().await;
        cx.editor(|editor, _, _| {
            assert!(!editor.hover_state.visible());
        });
    }

    #[gpui::test]
    async fn test_keyboard_hover_info_popover(cx: &mut gpui::TestAppContext) {
        init_test(cx, |_| {});

        let mut cx = EditorLspTestContext::new_rust(
            lsp::ServerCapabilities {
                hover_provider: Some(lsp::HoverProviderCapability::Simple(true)),
                ..Default::default()
            },
            cx,
        )
        .await;

        // Hover with keyboard has no delay
        cx.set_state(indoc! {"
            fˇn test() { println!(); }
        "});
        cx.update_editor(|editor, window, cx| hover(editor, &Hover, window, cx));
        let symbol_range = cx.lsp_range(indoc! {"
            «fn» test() { println!(); }
        "});

        cx.editor(|editor, _window, _cx| {
            assert!(!editor.hover_state.visible());

            assert_eq!(
                editor.hover_state.info_popovers.len(),
                0,
                "Expected no hovers but got but got: {:?}",
                editor.hover_state.info_popovers
            );
        });

        let mut requests =
            cx.handle_request::<lsp::request::HoverRequest, _, _>(move |_, _, _| async move {
                Ok(Some(lsp::Hover {
                    contents: lsp::HoverContents::Markup(lsp::MarkupContent {
                        kind: lsp::MarkupKind::Markdown,
                        value: "some other basic docs".to_string(),
                    }),
                    range: Some(symbol_range),
                }))
            });

        requests.next().await;
        cx.dispatch_action(Hover);

        cx.condition(|editor, _| editor.hover_state.visible()).await;
        cx.editor(|editor, _, cx| {
            assert_eq!(
                editor.hover_state.info_popovers.len(),
                1,
                "Expected exactly one hover but got: {:?}",
                editor.hover_state.info_popovers
            );

            let rendered_text = editor
                .hover_state
                .info_popovers
                .first()
                .unwrap()
                .get_rendered_text(cx);

            assert_eq!(rendered_text, "some other basic docs".to_string())
        });
    }

    #[gpui::test]
    async fn test_empty_hovers_filtered(cx: &mut gpui::TestAppContext) {
        init_test(cx, |_| {});

        let mut cx = EditorLspTestContext::new_rust(
            lsp::ServerCapabilities {
                hover_provider: Some(lsp::HoverProviderCapability::Simple(true)),
                ..Default::default()
            },
            cx,
        )
        .await;

        // Hover with keyboard has no delay
        cx.set_state(indoc! {"
            fˇn test() { println!(); }
        "});
        cx.update_editor(|editor, window, cx| hover(editor, &Hover, window, cx));
        let symbol_range = cx.lsp_range(indoc! {"
            «fn» test() { println!(); }
        "});
        cx.handle_request::<lsp::request::HoverRequest, _, _>(move |_, _, _| async move {
            Ok(Some(lsp::Hover {
                contents: lsp::HoverContents::Array(vec![
                    lsp::MarkedString::String("regular text for hover to show".to_string()),
                    lsp::MarkedString::String("".to_string()),
                    lsp::MarkedString::LanguageString(lsp::LanguageString {
                        language: "Rust".to_string(),
                        value: "".to_string(),
                    }),
                ]),
                range: Some(symbol_range),
            }))
        })
        .next()
        .await;
        cx.dispatch_action(Hover);

        cx.condition(|editor, _| editor.hover_state.visible()).await;
        cx.editor(|editor, _, cx| {
            assert_eq!(
                editor.hover_state.info_popovers.len(),
                1,
                "Expected exactly one hover but got: {:?}",
                editor.hover_state.info_popovers
            );
            let rendered_text = editor
                .hover_state
                .info_popovers
                .first()
                .unwrap()
                .get_rendered_text(cx);

            assert_eq!(
                rendered_text,
                "regular text for hover to show".to_string(),
                "No empty string hovers should be shown"
            );
        });
    }

    #[gpui::test]
    async fn test_line_ends_trimmed(cx: &mut gpui::TestAppContext) {
        init_test(cx, |_| {});

        let mut cx = EditorLspTestContext::new_rust(
            lsp::ServerCapabilities {
                hover_provider: Some(lsp::HoverProviderCapability::Simple(true)),
                ..Default::default()
            },
            cx,
        )
        .await;

        // Hover with keyboard has no delay
        cx.set_state(indoc! {"
            fˇn test() { println!(); }
        "});
        cx.update_editor(|editor, window, cx| hover(editor, &Hover, window, cx));
        let symbol_range = cx.lsp_range(indoc! {"
            «fn» test() { println!(); }
        "});

        let code_str = "\nlet hovered_point: Vector2F // size = 8, align = 0x4\n";
        let markdown_string = format!("\n```rust\n{code_str}```");

        let closure_markdown_string = markdown_string.clone();
        cx.handle_request::<lsp::request::HoverRequest, _, _>(move |_, _, _| {
            let future_markdown_string = closure_markdown_string.clone();
            async move {
                Ok(Some(lsp::Hover {
                    contents: lsp::HoverContents::Markup(lsp::MarkupContent {
                        kind: lsp::MarkupKind::Markdown,
                        value: future_markdown_string,
                    }),
                    range: Some(symbol_range),
                }))
            }
        })
        .next()
        .await;

        cx.dispatch_action(Hover);

        cx.condition(|editor, _| editor.hover_state.visible()).await;
        cx.editor(|editor, _, cx| {
            assert_eq!(
                editor.hover_state.info_popovers.len(),
                1,
                "Expected exactly one hover but got: {:?}",
                editor.hover_state.info_popovers
            );
            let rendered_text = editor
                .hover_state
                .info_popovers
                .first()
                .unwrap()
                .get_rendered_text(cx);

            assert_eq!(
                rendered_text, code_str,
                "Should not have extra line breaks at end of rendered hover"
            );
        });
    }

    #[gpui::test]
    async fn test_hover_diagnostic_and_info_popovers(cx: &mut gpui::TestAppContext) {
        init_test(cx, |_| {});

        let mut cx = EditorLspTestContext::new_rust(
            lsp::ServerCapabilities {
                hover_provider: Some(lsp::HoverProviderCapability::Simple(true)),
                ..Default::default()
            },
            cx,
        )
        .await;

        // Hover with just diagnostic, pops DiagnosticPopover immediately and then
        // info popover once request completes
        cx.set_state(indoc! {"
            fn teˇst() { println!(); }
        "});

        // Send diagnostic to client
        let range = cx.text_anchor_range(indoc! {"
            fn «test»() { println!(); }
        "});
        cx.update_buffer(|buffer, cx| {
            let snapshot = buffer.text_snapshot();
            let set = DiagnosticSet::from_sorted_entries(
                vec![DiagnosticEntry {
                    range,
                    diagnostic: Diagnostic {
                        message: "A test diagnostic message.".to_string(),
                        ..Default::default()
                    },
                }],
                &snapshot,
            );
            buffer.update_diagnostics(LanguageServerId(0), set, cx);
        });

        // Hover pops diagnostic immediately
        cx.update_editor(|editor, window, cx| hover(editor, &Hover, window, cx));
        cx.background_executor.run_until_parked();

        cx.editor(|Editor { hover_state, .. }, _, _| {
            assert!(
                hover_state.diagnostic_popover.is_some() && hover_state.info_popovers.is_empty()
            )
        });

        // Info Popover shows after request responded to
        let range = cx.lsp_range(indoc! {"
            fn «test»() { println!(); }
        "});
        cx.handle_request::<lsp::request::HoverRequest, _, _>(move |_, _, _| async move {
            Ok(Some(lsp::Hover {
                contents: lsp::HoverContents::Markup(lsp::MarkupContent {
                    kind: lsp::MarkupKind::Markdown,
                    value: "some new docs".to_string(),
                }),
                range: Some(range),
            }))
        });
        cx.background_executor
            .advance_clock(Duration::from_millis(get_hover_popover_delay(&cx) + 100));

        cx.background_executor.run_until_parked();
        cx.editor(|Editor { hover_state, .. }, _, _| {
            hover_state.diagnostic_popover.is_some() && hover_state.info_task.is_some()
        });
    }

    #[gpui::test]
    // https://github.com/zed-industries/zed/issues/15498
    async fn test_info_hover_with_hrs(cx: &mut gpui::TestAppContext) {
        init_test(cx, |_| {});

        let mut cx = EditorLspTestContext::new_rust(
            lsp::ServerCapabilities {
                hover_provider: Some(lsp::HoverProviderCapability::Simple(true)),
                ..Default::default()
            },
            cx,
        )
        .await;

        cx.set_state(indoc! {"
            fn fuˇnc(abc def: i32) -> u32 {
            }
        "});

        cx.lsp.handle_request::<lsp::request::HoverRequest, _, _>({
            |_, _| async move {
                Ok(Some(lsp::Hover {
                    contents: lsp::HoverContents::Markup(lsp::MarkupContent {
                        kind: lsp::MarkupKind::Markdown,
                        value: indoc!(
                            r#"
                    ### function `errands_data_read`

                    ---
                    → `char *`
                    Function to read a file into a string

                    ---
                    ```cpp
                    static char *errands_data_read()
                    ```
                    "#
                        )
                        .to_string(),
                    }),
                    range: None,
                }))
            }
        });
        cx.update_editor(|editor, window, cx| hover(editor, &Default::default(), window, cx));
        cx.run_until_parked();

        cx.update_editor(|editor, _, cx| {
            let popover = editor.hover_state.info_popovers.first().unwrap();
            let content = popover.get_rendered_text(cx);

            assert!(content.contains("Function to read a file"));
        });
    }

    #[gpui::test]
    async fn test_hover_inlay_label_parts(cx: &mut gpui::TestAppContext) {
        init_test(cx, |settings| {
            settings.defaults.inlay_hints = Some(InlayHintSettings {
                enabled: true,
                edit_debounce_ms: 0,
                scroll_debounce_ms: 0,
                show_type_hints: true,
                show_parameter_hints: true,
                show_other_hints: true,
                show_background: false,
            })
        });

        let mut cx = EditorLspTestContext::new_rust(
            lsp::ServerCapabilities {
                inlay_hint_provider: Some(lsp::OneOf::Right(
                    lsp::InlayHintServerCapabilities::Options(lsp::InlayHintOptions {
                        resolve_provider: Some(true),
                        ..Default::default()
                    }),
                )),
                ..Default::default()
            },
            cx,
        )
        .await;

        cx.set_state(indoc! {"
            struct TestStruct;

            // ==================

            struct TestNewType<T>(T);

            fn main() {
                let variableˇ = TestNewType(TestStruct);
            }
        "});

        let hint_start_offset = cx.ranges(indoc! {"
            struct TestStruct;

            // ==================

            struct TestNewType<T>(T);

            fn main() {
                let variableˇ = TestNewType(TestStruct);
            }
        "})[0]
            .start;
        let hint_position = cx.to_lsp(hint_start_offset);
        let new_type_target_range = cx.lsp_range(indoc! {"
            struct TestStruct;

            // ==================

            struct «TestNewType»<T>(T);

            fn main() {
                let variable = TestNewType(TestStruct);
            }
        "});
        let struct_target_range = cx.lsp_range(indoc! {"
            struct «TestStruct»;

            // ==================

            struct TestNewType<T>(T);

            fn main() {
                let variable = TestNewType(TestStruct);
            }
        "});

        let uri = cx.buffer_lsp_url.clone();
        let new_type_label = "TestNewType";
        let struct_label = "TestStruct";
        let entire_hint_label = ": TestNewType<TestStruct>";
        let closure_uri = uri.clone();
        cx.lsp
            .handle_request::<lsp::request::InlayHintRequest, _, _>(move |params, _| {
                let task_uri = closure_uri.clone();
                async move {
                    assert_eq!(params.text_document.uri, task_uri);
                    Ok(Some(vec![lsp::InlayHint {
                        position: hint_position,
                        label: lsp::InlayHintLabel::LabelParts(vec![lsp::InlayHintLabelPart {
                            value: entire_hint_label.to_string(),
                            ..Default::default()
                        }]),
                        kind: Some(lsp::InlayHintKind::TYPE),
                        text_edits: None,
                        tooltip: None,
                        padding_left: Some(false),
                        padding_right: Some(false),
                        data: None,
                    }]))
                }
            })
            .next()
            .await;
        cx.background_executor.run_until_parked();
        cx.update_editor(|editor, _, cx| {
            let expected_layers = vec![entire_hint_label.to_string()];
            assert_eq!(expected_layers, cached_hint_labels(editor));
            assert_eq!(expected_layers, visible_hint_labels(editor, cx));
        });

        let inlay_range = cx
            .ranges(indoc! {"
                struct TestStruct;

                // ==================

                struct TestNewType<T>(T);

                fn main() {
                    let variable« »= TestNewType(TestStruct);
                }
        "})
            .first()
            .cloned()
            .unwrap();
        let new_type_hint_part_hover_position = cx.update_editor(|editor, window, cx| {
            let snapshot = editor.snapshot(window, cx);
            let previous_valid = inlay_range.start.to_display_point(&snapshot);
            let next_valid = inlay_range.end.to_display_point(&snapshot);
            assert_eq!(previous_valid.row(), next_valid.row());
            assert!(previous_valid.column() < next_valid.column());
            let exact_unclipped = DisplayPoint::new(
                previous_valid.row(),
                previous_valid.column()
                    + (entire_hint_label.find(new_type_label).unwrap() + new_type_label.len() / 2)
                        as u32,
            );
            PointForPosition {
                previous_valid,
                next_valid,
                exact_unclipped,
                column_overshoot_after_line_end: 0,
            }
        });
        cx.update_editor(|editor, window, cx| {
            update_inlay_link_and_hover_points(
                &editor.snapshot(window, cx),
                new_type_hint_part_hover_position,
                editor,
                true,
                false,
                window,
                cx,
            );
        });

        let resolve_closure_uri = uri.clone();
        cx.lsp
            .handle_request::<lsp::request::InlayHintResolveRequest, _, _>(
                move |mut hint_to_resolve, _| {
                    let mut resolved_hint_positions = BTreeSet::new();
                    let task_uri = resolve_closure_uri.clone();
                    async move {
                        let inserted = resolved_hint_positions.insert(hint_to_resolve.position);
                        assert!(inserted, "Hint {hint_to_resolve:?} was resolved twice");

                        // `: TestNewType<TestStruct>`
                        hint_to_resolve.label = lsp::InlayHintLabel::LabelParts(vec![
                            lsp::InlayHintLabelPart {
                                value: ": ".to_string(),
                                ..Default::default()
                            },
                            lsp::InlayHintLabelPart {
                                value: new_type_label.to_string(),
                                location: Some(lsp::Location {
                                    uri: task_uri.clone(),
                                    range: new_type_target_range,
                                }),
                                tooltip: Some(lsp::InlayHintLabelPartTooltip::String(format!(
                                    "A tooltip for `{new_type_label}`"
                                ))),
                                ..Default::default()
                            },
                            lsp::InlayHintLabelPart {
                                value: "<".to_string(),
                                ..Default::default()
                            },
                            lsp::InlayHintLabelPart {
                                value: struct_label.to_string(),
                                location: Some(lsp::Location {
                                    uri: task_uri,
                                    range: struct_target_range,
                                }),
                                tooltip: Some(lsp::InlayHintLabelPartTooltip::MarkupContent(
                                    lsp::MarkupContent {
                                        kind: lsp::MarkupKind::Markdown,
                                        value: format!("A tooltip for `{struct_label}`"),
                                    },
                                )),
                                ..Default::default()
                            },
                            lsp::InlayHintLabelPart {
                                value: ">".to_string(),
                                ..Default::default()
                            },
                        ]);

                        Ok(hint_to_resolve)
                    }
                },
            )
            .next()
            .await;
        cx.background_executor.run_until_parked();

        cx.update_editor(|editor, window, cx| {
            update_inlay_link_and_hover_points(
                &editor.snapshot(window, cx),
                new_type_hint_part_hover_position,
                editor,
                true,
                false,
                window,
                cx,
            );
        });
        cx.background_executor
            .advance_clock(Duration::from_millis(get_hover_popover_delay(&cx) + 100));
        cx.background_executor.run_until_parked();
        cx.update_editor(|editor, _, cx| {
            let hover_state = &editor.hover_state;
            assert!(
                hover_state.diagnostic_popover.is_none() && hover_state.info_popovers.len() == 1
            );
            let popover = hover_state.info_popovers.first().cloned().unwrap();
            let buffer_snapshot = editor.buffer().update(cx, |buffer, cx| buffer.snapshot(cx));
            assert_eq!(
                popover.symbol_range,
                RangeInEditor::Inlay(InlayHighlight {
                    inlay: InlayId::Hint(0),
                    inlay_position: buffer_snapshot.anchor_at(inlay_range.start, Bias::Right),
                    range: ": ".len()..": ".len() + new_type_label.len(),
                }),
                "Popover range should match the new type label part"
            );
            assert_eq!(
                popover.get_rendered_text(cx),
                format!("A tooltip for {new_type_label}"),
            );
        });

        let struct_hint_part_hover_position = cx.update_editor(|editor, window, cx| {
            let snapshot = editor.snapshot(window, cx);
            let previous_valid = inlay_range.start.to_display_point(&snapshot);
            let next_valid = inlay_range.end.to_display_point(&snapshot);
            assert_eq!(previous_valid.row(), next_valid.row());
            assert!(previous_valid.column() < next_valid.column());
            let exact_unclipped = DisplayPoint::new(
                previous_valid.row(),
                previous_valid.column()
                    + (entire_hint_label.find(struct_label).unwrap() + struct_label.len() / 2)
                        as u32,
            );
            PointForPosition {
                previous_valid,
                next_valid,
                exact_unclipped,
                column_overshoot_after_line_end: 0,
            }
        });
        cx.update_editor(|editor, window, cx| {
            update_inlay_link_and_hover_points(
                &editor.snapshot(window, cx),
                struct_hint_part_hover_position,
                editor,
                true,
                false,
                window,
                cx,
            );
        });
        cx.background_executor
            .advance_clock(Duration::from_millis(get_hover_popover_delay(&cx) + 100));
        cx.background_executor.run_until_parked();
        cx.update_editor(|editor, _, cx| {
            let hover_state = &editor.hover_state;
            assert!(
                hover_state.diagnostic_popover.is_none() && hover_state.info_popovers.len() == 1
            );
            let popover = hover_state.info_popovers.first().cloned().unwrap();
            let buffer_snapshot = editor.buffer().update(cx, |buffer, cx| buffer.snapshot(cx));
            assert_eq!(
                popover.symbol_range,
                RangeInEditor::Inlay(InlayHighlight {
                    inlay: InlayId::Hint(0),
                    inlay_position: buffer_snapshot.anchor_at(inlay_range.start, Bias::Right),
                    range: ": ".len() + new_type_label.len() + "<".len()
                        ..": ".len() + new_type_label.len() + "<".len() + struct_label.len(),
                }),
                "Popover range should match the struct label part"
            );
            assert_eq!(
                popover.get_rendered_text(cx),
                format!("A tooltip for {struct_label}"),
                "Rendered markdown element should remove backticks from text"
            );
        });
    }
}
