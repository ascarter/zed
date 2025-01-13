use super::*;
use git::diff::DiffHunkStatus;
use gpui::{AppContext, Context, TestAppContext};
use indoc::indoc;
use language::{Buffer, Rope};
use parking_lot::RwLock;
use rand::prelude::*;
use settings::SettingsStore;
use std::env;
use util::test::sample_text;

#[ctor::ctor]
fn init_logger() {
    if std::env::var("RUST_LOG").is_ok() {
        env_logger::init();
    }
}

#[gpui::test]
fn test_singleton(cx: &mut AppContext) {
    let buffer = cx.new_model(|cx| Buffer::local(sample_text(6, 6, 'a'), cx));
    let multibuffer = cx.new_model(|cx| MultiBuffer::singleton(buffer.clone(), cx));

    let snapshot = multibuffer.read(cx).snapshot(cx);
    assert_eq!(snapshot.text(), buffer.read(cx).text());

    assert_eq!(
        snapshot
            .row_infos(MultiBufferRow(0))
            .map(|info| info.buffer_row)
            .collect::<Vec<_>>(),
        (0..buffer.read(cx).row_count())
            .map(Some)
            .collect::<Vec<_>>()
    );

    buffer.update(cx, |buffer, cx| buffer.edit([(1..3, "XXX\n")], None, cx));
    let snapshot = multibuffer.read(cx).snapshot(cx);

    assert_eq!(snapshot.text(), buffer.read(cx).text());
    assert_eq!(
        snapshot
            .row_infos(MultiBufferRow(0))
            .map(|info| info.buffer_row)
            .collect::<Vec<_>>(),
        (0..buffer.read(cx).row_count())
            .map(Some)
            .collect::<Vec<_>>()
    );
}

#[gpui::test]
fn test_remote(cx: &mut AppContext) {
    let host_buffer = cx.new_model(|cx| Buffer::local("a", cx));
    let guest_buffer = cx.new_model(|cx| {
        let state = host_buffer.read(cx).to_proto(cx);
        let ops = cx
            .background_executor()
            .block(host_buffer.read(cx).serialize_ops(None, cx));
        let mut buffer = Buffer::from_proto(1, Capability::ReadWrite, state, None).unwrap();
        buffer.apply_ops(
            ops.into_iter()
                .map(|op| language::proto::deserialize_operation(op).unwrap()),
            cx,
        );
        buffer
    });
    let multibuffer = cx.new_model(|cx| MultiBuffer::singleton(guest_buffer.clone(), cx));
    let snapshot = multibuffer.read(cx).snapshot(cx);
    assert_eq!(snapshot.text(), "a");

    guest_buffer.update(cx, |buffer, cx| buffer.edit([(1..1, "b")], None, cx));
    let snapshot = multibuffer.read(cx).snapshot(cx);
    assert_eq!(snapshot.text(), "ab");

    guest_buffer.update(cx, |buffer, cx| buffer.edit([(2..2, "c")], None, cx));
    let snapshot = multibuffer.read(cx).snapshot(cx);
    assert_eq!(snapshot.text(), "abc");
}

#[gpui::test]
fn test_excerpt_boundaries_and_clipping(cx: &mut AppContext) {
    let buffer_1 = cx.new_model(|cx| Buffer::local(sample_text(6, 6, 'a'), cx));
    let buffer_2 = cx.new_model(|cx| Buffer::local(sample_text(6, 6, 'g'), cx));
    let multibuffer = cx.new_model(|_| MultiBuffer::new(Capability::ReadWrite));

    let events = Arc::new(RwLock::new(Vec::<Event>::new()));
    multibuffer.update(cx, |_, cx| {
        let events = events.clone();
        cx.subscribe(&multibuffer, move |_, _, event, _| {
            if let Event::Edited { .. } = event {
                events.write().push(event.clone())
            }
        })
        .detach();
    });

    let subscription = multibuffer.update(cx, |multibuffer, cx| {
        let subscription = multibuffer.subscribe();
        multibuffer.push_excerpts(
            buffer_1.clone(),
            [ExcerptRange {
                context: Point::new(1, 2)..Point::new(2, 5),
                primary: None,
            }],
            cx,
        );
        assert_eq!(
            subscription.consume().into_inner(),
            [Edit {
                old: 0..0,
                new: 0..10
            }]
        );

        multibuffer.push_excerpts(
            buffer_1.clone(),
            [ExcerptRange {
                context: Point::new(3, 3)..Point::new(4, 4),
                primary: None,
            }],
            cx,
        );
        multibuffer.push_excerpts(
            buffer_2.clone(),
            [ExcerptRange {
                context: Point::new(3, 1)..Point::new(3, 3),
                primary: None,
            }],
            cx,
        );
        assert_eq!(
            subscription.consume().into_inner(),
            [Edit {
                old: 10..10,
                new: 10..22
            }]
        );

        subscription
    });

    // Adding excerpts emits an edited event.
    assert_eq!(
        events.read().as_slice(),
        &[
            Event::Edited {
                singleton_buffer_edited: false,
                edited_buffer: None,
            },
            Event::Edited {
                singleton_buffer_edited: false,
                edited_buffer: None,
            },
            Event::Edited {
                singleton_buffer_edited: false,
                edited_buffer: None,
            }
        ]
    );

    let snapshot = multibuffer.read(cx).snapshot(cx);
    assert_eq!(
        snapshot.text(),
        indoc!(
            "
            bbbb
            ccccc
            ddd
            eeee
            jj"
        ),
    );
    assert_eq!(
        snapshot
            .row_infos(MultiBufferRow(0))
            .map(|info| info.buffer_row)
            .collect::<Vec<_>>(),
        [Some(1), Some(2), Some(3), Some(4), Some(3)]
    );
    assert_eq!(
        snapshot
            .row_infos(MultiBufferRow(2))
            .map(|info| info.buffer_row)
            .collect::<Vec<_>>(),
        [Some(3), Some(4), Some(3)]
    );
    assert_eq!(
        snapshot
            .row_infos(MultiBufferRow(4))
            .map(|info| info.buffer_row)
            .collect::<Vec<_>>(),
        [Some(3)]
    );
    assert_eq!(
        snapshot
            .row_infos(MultiBufferRow(5))
            .map(|info| info.buffer_row)
            .collect::<Vec<_>>(),
        []
    );

    assert_eq!(
        boundaries_in_range(Point::new(0, 0)..Point::new(4, 2), &snapshot),
        &[
            (MultiBufferRow(0), "bbbb\nccccc".to_string(), true),
            (MultiBufferRow(2), "ddd\neeee".to_string(), false),
            (MultiBufferRow(4), "jj".to_string(), true),
        ]
    );
    assert_eq!(
        boundaries_in_range(Point::new(0, 0)..Point::new(2, 0), &snapshot),
        &[(MultiBufferRow(0), "bbbb\nccccc".to_string(), true)]
    );
    assert_eq!(
        boundaries_in_range(Point::new(1, 0)..Point::new(1, 5), &snapshot),
        &[]
    );
    assert_eq!(
        boundaries_in_range(Point::new(1, 0)..Point::new(2, 0), &snapshot),
        &[]
    );
    assert_eq!(
        boundaries_in_range(Point::new(1, 0)..Point::new(4, 0), &snapshot),
        &[(MultiBufferRow(2), "ddd\neeee".to_string(), false)]
    );
    assert_eq!(
        boundaries_in_range(Point::new(1, 0)..Point::new(4, 0), &snapshot),
        &[(MultiBufferRow(2), "ddd\neeee".to_string(), false)]
    );
    assert_eq!(
        boundaries_in_range(Point::new(2, 0)..Point::new(3, 0), &snapshot),
        &[(MultiBufferRow(2), "ddd\neeee".to_string(), false)]
    );
    assert_eq!(
        boundaries_in_range(Point::new(4, 0)..Point::new(4, 2), &snapshot),
        &[(MultiBufferRow(4), "jj".to_string(), true)]
    );
    assert_eq!(
        boundaries_in_range(Point::new(4, 2)..Point::new(4, 2), &snapshot),
        &[]
    );

    buffer_1.update(cx, |buffer, cx| {
        let text = "\n";
        buffer.edit(
            [
                (Point::new(0, 0)..Point::new(0, 0), text),
                (Point::new(2, 1)..Point::new(2, 3), text),
            ],
            None,
            cx,
        );
    });

    let snapshot = multibuffer.read(cx).snapshot(cx);
    assert_eq!(
        snapshot.text(),
        concat!(
            "bbbb\n", // Preserve newlines
            "c\n",    //
            "cc\n",   //
            "ddd\n",  //
            "eeee\n", //
            "jj"      //
        )
    );

    assert_eq!(
        subscription.consume().into_inner(),
        [Edit {
            old: 6..8,
            new: 6..7
        }]
    );

    let snapshot = multibuffer.read(cx).snapshot(cx);
    assert_eq!(
        snapshot.clip_point(Point::new(0, 5), Bias::Left),
        Point::new(0, 4)
    );
    assert_eq!(
        snapshot.clip_point(Point::new(0, 5), Bias::Right),
        Point::new(0, 4)
    );
    assert_eq!(
        snapshot.clip_point(Point::new(5, 1), Bias::Right),
        Point::new(5, 1)
    );
    assert_eq!(
        snapshot.clip_point(Point::new(5, 2), Bias::Right),
        Point::new(5, 2)
    );
    assert_eq!(
        snapshot.clip_point(Point::new(5, 3), Bias::Right),
        Point::new(5, 2)
    );

    let snapshot = multibuffer.update(cx, |multibuffer, cx| {
        let (buffer_2_excerpt_id, _) = multibuffer.excerpts_for_buffer(&buffer_2, cx)[0].clone();
        multibuffer.remove_excerpts([buffer_2_excerpt_id], cx);
        multibuffer.snapshot(cx)
    });

    assert_eq!(
        snapshot.text(),
        concat!(
            "bbbb\n", // Preserve newlines
            "c\n",    //
            "cc\n",   //
            "ddd\n",  //
            "eeee",   //
        )
    );

    fn boundaries_in_range(
        range: Range<Point>,
        snapshot: &MultiBufferSnapshot,
    ) -> Vec<(MultiBufferRow, String, bool)> {
        snapshot
            .excerpt_boundaries_in_range(range)
            .filter_map(|boundary| {
                let starts_new_buffer = boundary.starts_new_buffer();
                boundary.next.map(|next| {
                    (
                        boundary.row,
                        next.buffer
                            .text_for_range(next.range.context)
                            .collect::<String>(),
                        starts_new_buffer,
                    )
                })
            })
            .collect::<Vec<_>>()
    }
}

#[gpui::test]
fn test_diff_boundary_anchors(cx: &mut AppContext) {
    let base_text = "one\ntwo\nthree\n";
    let text = "one\nthree\n";
    let buffer = cx.new_model(|cx| Buffer::local(text, cx));
    let snapshot = buffer.read(cx).snapshot();
    let change_set = cx.new_model(|cx| {
        let mut change_set = BufferChangeSet::new(&buffer, cx);
        change_set.recalculate_diff_sync(base_text.into(), snapshot.text, true, cx);
        change_set
    });
    let multibuffer = cx.new_model(|cx| MultiBuffer::singleton(buffer, cx));
    multibuffer.update(cx, |multibuffer, cx| {
        multibuffer.add_change_set(change_set, cx)
    });

    let (before, after) = multibuffer.update(cx, |multibuffer, cx| {
        let before = multibuffer.snapshot(cx).anchor_before(Point::new(1, 0));
        let after = multibuffer.snapshot(cx).anchor_after(Point::new(1, 0));
        multibuffer.set_all_hunks_expanded(cx);
        (before, after)
    });
    cx.background_executor().run_until_parked();

    let snapshot = multibuffer.read(cx).snapshot(cx);
    let actual_text = snapshot.text();
    let actual_row_infos = snapshot.row_infos(MultiBufferRow(0)).collect::<Vec<_>>();
    let actual_diff = format_diff(&actual_text, &actual_row_infos);
    pretty_assertions::assert_eq!(
        actual_diff,
        indoc! {
            "  one
             - two
               three
             "
        },
    );

    multibuffer.update(cx, |multibuffer, cx| {
        let snapshot = multibuffer.snapshot(cx);
        assert_eq!(before.to_point(&snapshot), Point::new(1, 0));
        assert_eq!(after.to_point(&snapshot), Point::new(2, 0));
        assert_eq!(
            vec![Point::new(1, 0), Point::new(2, 0),],
            snapshot.summaries_for_anchors::<Point, _>(&[before, after]),
        )
    })
}

#[gpui::test]
fn test_diff_hunks_in_range(cx: &mut AppContext) {
    let base_text = "one\ntwo\nthree\nfour\nfive\n";
    let text = "one\nthree\nfive\n";
    let buffer = cx.new_model(|cx| Buffer::local(text, cx));
    let change_set = cx.new_model(|cx| {
        let mut change_set = BufferChangeSet::new(&buffer, cx);
        let snapshot = buffer.read(cx).snapshot();
        change_set.recalculate_diff_sync(base_text.into(), snapshot.text, true, cx);
        change_set
    });
    let multibuffer = cx.new_model(|cx| MultiBuffer::singleton(buffer, cx));
    multibuffer.update(cx, |multibuffer, cx| {
        multibuffer.set_all_hunks_expanded(cx);
        multibuffer.add_change_set(change_set, cx);
    });
    cx.background_executor().run_until_parked();

    let snapshot = multibuffer.read(cx).snapshot(cx);
    let actual_text = snapshot.text();
    let actual_row_infos = snapshot.row_infos(MultiBufferRow(0)).collect::<Vec<_>>();
    let actual_diff = format_diff(&actual_text, &actual_row_infos);
    pretty_assertions::assert_eq!(
        actual_diff,
        indoc! {
            "  one
             - two
               three
             - four
               five
             "
        },
    );

    assert_eq!(
        snapshot
            .diff_hunks_in_range(Point::new(1, 0)..Point::MAX)
            .map(|hunk| hunk.row_range.start.0..hunk.row_range.end.0)
            .collect::<Vec<_>>(),
        vec![1..2, 3..4]
    );

    assert_eq!(
        snapshot
            .diff_hunk_before(Point::new(1, 1))
            .map(|hunk| hunk.row_range.start.0..hunk.row_range.end.0),
        None,
    );
    assert_eq!(
        snapshot
            .diff_hunk_before(Point::new(2, 1))
            .map(|hunk| hunk.row_range.start.0..hunk.row_range.end.0),
        Some(1..2)
    );
    assert_eq!(
        snapshot
            .diff_hunk_before(Point::new(4, 0))
            .map(|hunk| hunk.row_range.start.0..hunk.row_range.end.0),
        Some(3..4)
    );
}

#[gpui::test]
fn test_excerpt_events(cx: &mut AppContext) {
    let buffer_1 = cx.new_model(|cx| Buffer::local(sample_text(10, 3, 'a'), cx));
    let buffer_2 = cx.new_model(|cx| Buffer::local(sample_text(10, 3, 'm'), cx));

    let leader_multibuffer = cx.new_model(|_| MultiBuffer::new(Capability::ReadWrite));
    let follower_multibuffer = cx.new_model(|_| MultiBuffer::new(Capability::ReadWrite));
    let follower_edit_event_count = Arc::new(RwLock::new(0));

    follower_multibuffer.update(cx, |_, cx| {
        let follower_edit_event_count = follower_edit_event_count.clone();
        cx.subscribe(
            &leader_multibuffer,
            move |follower, _, event, cx| match event.clone() {
                Event::ExcerptsAdded {
                    buffer,
                    predecessor,
                    excerpts,
                } => follower.insert_excerpts_with_ids_after(predecessor, buffer, excerpts, cx),
                Event::ExcerptsRemoved { ids } => follower.remove_excerpts(ids, cx),
                Event::Edited { .. } => {
                    *follower_edit_event_count.write() += 1;
                }
                _ => {}
            },
        )
        .detach();
    });

    leader_multibuffer.update(cx, |leader, cx| {
        leader.push_excerpts(
            buffer_1.clone(),
            [
                ExcerptRange {
                    context: 0..8,
                    primary: None,
                },
                ExcerptRange {
                    context: 12..16,
                    primary: None,
                },
            ],
            cx,
        );
        leader.insert_excerpts_after(
            leader.excerpt_ids()[0],
            buffer_2.clone(),
            [
                ExcerptRange {
                    context: 0..5,
                    primary: None,
                },
                ExcerptRange {
                    context: 10..15,
                    primary: None,
                },
            ],
            cx,
        )
    });
    assert_eq!(
        leader_multibuffer.read(cx).snapshot(cx).text(),
        follower_multibuffer.read(cx).snapshot(cx).text(),
    );
    assert_eq!(*follower_edit_event_count.read(), 2);

    leader_multibuffer.update(cx, |leader, cx| {
        let excerpt_ids = leader.excerpt_ids();
        leader.remove_excerpts([excerpt_ids[1], excerpt_ids[3]], cx);
    });
    assert_eq!(
        leader_multibuffer.read(cx).snapshot(cx).text(),
        follower_multibuffer.read(cx).snapshot(cx).text(),
    );
    assert_eq!(*follower_edit_event_count.read(), 3);

    // Removing an empty set of excerpts is a noop.
    leader_multibuffer.update(cx, |leader, cx| {
        leader.remove_excerpts([], cx);
    });
    assert_eq!(
        leader_multibuffer.read(cx).snapshot(cx).text(),
        follower_multibuffer.read(cx).snapshot(cx).text(),
    );
    assert_eq!(*follower_edit_event_count.read(), 3);

    // Adding an empty set of excerpts is a noop.
    leader_multibuffer.update(cx, |leader, cx| {
        leader.push_excerpts::<usize>(buffer_2.clone(), [], cx);
    });
    assert_eq!(
        leader_multibuffer.read(cx).snapshot(cx).text(),
        follower_multibuffer.read(cx).snapshot(cx).text(),
    );
    assert_eq!(*follower_edit_event_count.read(), 3);

    leader_multibuffer.update(cx, |leader, cx| {
        leader.clear(cx);
    });
    assert_eq!(
        leader_multibuffer.read(cx).snapshot(cx).text(),
        follower_multibuffer.read(cx).snapshot(cx).text(),
    );
    assert_eq!(*follower_edit_event_count.read(), 4);
}

#[gpui::test]
fn test_expand_excerpts(cx: &mut AppContext) {
    let buffer = cx.new_model(|cx| Buffer::local(sample_text(20, 3, 'a'), cx));
    let multibuffer = cx.new_model(|_| MultiBuffer::new(Capability::ReadWrite));

    multibuffer.update(cx, |multibuffer, cx| {
        multibuffer.push_excerpts_with_context_lines(
            buffer.clone(),
            vec![
                // Note that in this test, this first excerpt
                // does not contain a new line
                Point::new(3, 2)..Point::new(3, 3),
                Point::new(7, 1)..Point::new(7, 3),
                Point::new(15, 0)..Point::new(15, 0),
            ],
            1,
            cx,
        )
    });

    let snapshot = multibuffer.read(cx).snapshot(cx);

    assert_eq!(
        snapshot.text(),
        concat!(
            "ccc\n", //
            "ddd\n", //
            "eee",   //
            "\n",    // End of excerpt
            "ggg\n", //
            "hhh\n", //
            "iii",   //
            "\n",    // End of excerpt
            "ooo\n", //
            "ppp\n", //
            "qqq",   // End of excerpt
        )
    );
    drop(snapshot);

    multibuffer.update(cx, |multibuffer, cx| {
        multibuffer.expand_excerpts(
            multibuffer.excerpt_ids(),
            1,
            ExpandExcerptDirection::UpAndDown,
            cx,
        )
    });

    let snapshot = multibuffer.read(cx).snapshot(cx);

    // Expanding context lines causes the line containing 'fff' to appear in two different excerpts.
    // We don't attempt to merge them, because removing the excerpt could create inconsistency with other layers
    // that are tracking excerpt ids.
    assert_eq!(
        snapshot.text(),
        concat!(
            "bbb\n", //
            "ccc\n", //
            "ddd\n", //
            "eee\n", //
            "fff\n", // End of excerpt
            "fff\n", //
            "ggg\n", //
            "hhh\n", //
            "iii\n", //
            "jjj\n", // End of excerpt
            "nnn\n", //
            "ooo\n", //
            "ppp\n", //
            "qqq\n", //
            "rrr",   // End of excerpt
        )
    );
}

#[gpui::test]
fn test_push_excerpts_with_context_lines(cx: &mut AppContext) {
    let buffer = cx.new_model(|cx| Buffer::local(sample_text(20, 3, 'a'), cx));
    let multibuffer = cx.new_model(|_| MultiBuffer::new(Capability::ReadWrite));
    let anchor_ranges = multibuffer.update(cx, |multibuffer, cx| {
        multibuffer.push_excerpts_with_context_lines(
            buffer.clone(),
            vec![
                // Note that in this test, this first excerpt
                // does contain a new line
                Point::new(3, 2)..Point::new(4, 2),
                Point::new(7, 1)..Point::new(7, 3),
                Point::new(15, 0)..Point::new(15, 0),
            ],
            2,
            cx,
        )
    });

    let snapshot = multibuffer.read(cx).snapshot(cx);
    assert_eq!(
        snapshot.text(),
        concat!(
            "bbb\n", // Preserve newlines
            "ccc\n", //
            "ddd\n", //
            "eee\n", //
            "fff\n", //
            "ggg\n", //
            "hhh\n", //
            "iii\n", //
            "jjj\n", //
            "nnn\n", //
            "ooo\n", //
            "ppp\n", //
            "qqq\n", //
            "rrr",   //
        )
    );

    assert_eq!(
        anchor_ranges
            .iter()
            .map(|range| range.to_point(&snapshot))
            .collect::<Vec<_>>(),
        vec![
            Point::new(2, 2)..Point::new(3, 2),
            Point::new(6, 1)..Point::new(6, 3),
            Point::new(11, 0)..Point::new(11, 0)
        ]
    );
}

#[gpui::test(iterations = 100)]
async fn test_push_multiple_excerpts_with_context_lines(cx: &mut TestAppContext) {
    let buffer_1 = cx.new_model(|cx| Buffer::local(sample_text(20, 3, 'a'), cx));
    let buffer_2 = cx.new_model(|cx| Buffer::local(sample_text(15, 4, 'a'), cx));
    let snapshot_1 = buffer_1.update(cx, |buffer, _| buffer.snapshot());
    let snapshot_2 = buffer_2.update(cx, |buffer, _| buffer.snapshot());
    let ranges_1 = vec![
        snapshot_1.anchor_before(Point::new(3, 2))..snapshot_1.anchor_before(Point::new(4, 2)),
        snapshot_1.anchor_before(Point::new(7, 1))..snapshot_1.anchor_before(Point::new(7, 3)),
        snapshot_1.anchor_before(Point::new(15, 0))..snapshot_1.anchor_before(Point::new(15, 0)),
    ];
    let ranges_2 = vec![
        snapshot_2.anchor_before(Point::new(2, 1))..snapshot_2.anchor_before(Point::new(3, 1)),
        snapshot_2.anchor_before(Point::new(10, 0))..snapshot_2.anchor_before(Point::new(10, 2)),
    ];

    let multibuffer = cx.new_model(|_| MultiBuffer::new(Capability::ReadWrite));
    let anchor_ranges = multibuffer
        .update(cx, |multibuffer, cx| {
            multibuffer.push_multiple_excerpts_with_context_lines(
                vec![(buffer_1.clone(), ranges_1), (buffer_2.clone(), ranges_2)],
                2,
                cx,
            )
        })
        .await;

    let snapshot = multibuffer.update(cx, |multibuffer, cx| multibuffer.snapshot(cx));
    assert_eq!(
        snapshot.text(),
        concat!(
            "bbb\n", // buffer_1
            "ccc\n", //
            "ddd\n", // <-- excerpt 1
            "eee\n", // <-- excerpt 1
            "fff\n", //
            "ggg\n", //
            "hhh\n", // <-- excerpt 2
            "iii\n", //
            "jjj\n", //
            //
            "nnn\n", //
            "ooo\n", //
            "ppp\n", // <-- excerpt 3
            "qqq\n", //
            "rrr\n", //
            //
            "aaaa\n", // buffer 2
            "bbbb\n", //
            "cccc\n", // <-- excerpt 4
            "dddd\n", // <-- excerpt 4
            "eeee\n", //
            "ffff\n", //
            //
            "iiii\n", //
            "jjjj\n", //
            "kkkk\n", // <-- excerpt 5
            "llll\n", //
            "mmmm",   //
        )
    );

    assert_eq!(
        anchor_ranges
            .iter()
            .map(|range| range.to_point(&snapshot))
            .collect::<Vec<_>>(),
        vec![
            Point::new(2, 2)..Point::new(3, 2),
            Point::new(6, 1)..Point::new(6, 3),
            Point::new(11, 0)..Point::new(11, 0),
            Point::new(16, 1)..Point::new(17, 1),
            Point::new(22, 0)..Point::new(22, 2)
        ]
    );
}

#[gpui::test]
fn test_empty_multibuffer(cx: &mut AppContext) {
    let multibuffer = cx.new_model(|_| MultiBuffer::new(Capability::ReadWrite));

    let snapshot = multibuffer.read(cx).snapshot(cx);
    assert_eq!(snapshot.text(), "");
    assert_eq!(
        snapshot
            .row_infos(MultiBufferRow(0))
            .map(|info| info.buffer_row)
            .collect::<Vec<_>>(),
        &[Some(0)]
    );
    assert_eq!(
        snapshot
            .row_infos(MultiBufferRow(1))
            .map(|info| info.buffer_row)
            .collect::<Vec<_>>(),
        &[]
    );
}

#[gpui::test]
fn test_singleton_multibuffer_anchors(cx: &mut AppContext) {
    let buffer = cx.new_model(|cx| Buffer::local("abcd", cx));
    let multibuffer = cx.new_model(|cx| MultiBuffer::singleton(buffer.clone(), cx));
    let old_snapshot = multibuffer.read(cx).snapshot(cx);
    buffer.update(cx, |buffer, cx| {
        buffer.edit([(0..0, "X")], None, cx);
        buffer.edit([(5..5, "Y")], None, cx);
    });
    let new_snapshot = multibuffer.read(cx).snapshot(cx);

    assert_eq!(old_snapshot.text(), "abcd");
    assert_eq!(new_snapshot.text(), "XabcdY");

    assert_eq!(old_snapshot.anchor_before(0).to_offset(&new_snapshot), 0);
    assert_eq!(old_snapshot.anchor_after(0).to_offset(&new_snapshot), 1);
    assert_eq!(old_snapshot.anchor_before(4).to_offset(&new_snapshot), 5);
    assert_eq!(old_snapshot.anchor_after(4).to_offset(&new_snapshot), 6);
}

#[gpui::test]
fn test_multibuffer_anchors(cx: &mut AppContext) {
    let buffer_1 = cx.new_model(|cx| Buffer::local("abcd", cx));
    let buffer_2 = cx.new_model(|cx| Buffer::local("efghi", cx));
    let multibuffer = cx.new_model(|cx| {
        let mut multibuffer = MultiBuffer::new(Capability::ReadWrite);
        multibuffer.push_excerpts(
            buffer_1.clone(),
            [ExcerptRange {
                context: 0..4,
                primary: None,
            }],
            cx,
        );
        multibuffer.push_excerpts(
            buffer_2.clone(),
            [ExcerptRange {
                context: 0..5,
                primary: None,
            }],
            cx,
        );
        multibuffer
    });
    let old_snapshot = multibuffer.read(cx).snapshot(cx);

    assert_eq!(old_snapshot.anchor_before(0).to_offset(&old_snapshot), 0);
    assert_eq!(old_snapshot.anchor_after(0).to_offset(&old_snapshot), 0);
    assert_eq!(Anchor::min().to_offset(&old_snapshot), 0);
    assert_eq!(Anchor::min().to_offset(&old_snapshot), 0);
    assert_eq!(Anchor::max().to_offset(&old_snapshot), 10);
    assert_eq!(Anchor::max().to_offset(&old_snapshot), 10);

    buffer_1.update(cx, |buffer, cx| {
        buffer.edit([(0..0, "W")], None, cx);
        buffer.edit([(5..5, "X")], None, cx);
    });
    buffer_2.update(cx, |buffer, cx| {
        buffer.edit([(0..0, "Y")], None, cx);
        buffer.edit([(6..6, "Z")], None, cx);
    });
    let new_snapshot = multibuffer.read(cx).snapshot(cx);

    assert_eq!(old_snapshot.text(), "abcd\nefghi");
    assert_eq!(new_snapshot.text(), "WabcdX\nYefghiZ");

    assert_eq!(old_snapshot.anchor_before(0).to_offset(&new_snapshot), 0);
    assert_eq!(old_snapshot.anchor_after(0).to_offset(&new_snapshot), 1);
    assert_eq!(old_snapshot.anchor_before(1).to_offset(&new_snapshot), 2);
    assert_eq!(old_snapshot.anchor_after(1).to_offset(&new_snapshot), 2);
    assert_eq!(old_snapshot.anchor_before(2).to_offset(&new_snapshot), 3);
    assert_eq!(old_snapshot.anchor_after(2).to_offset(&new_snapshot), 3);
    assert_eq!(old_snapshot.anchor_before(5).to_offset(&new_snapshot), 7);
    assert_eq!(old_snapshot.anchor_after(5).to_offset(&new_snapshot), 8);
    assert_eq!(old_snapshot.anchor_before(10).to_offset(&new_snapshot), 13);
    assert_eq!(old_snapshot.anchor_after(10).to_offset(&new_snapshot), 14);
}

#[gpui::test]
fn test_resolving_anchors_after_replacing_their_excerpts(cx: &mut AppContext) {
    let buffer_1 = cx.new_model(|cx| Buffer::local("abcd", cx));
    let buffer_2 = cx.new_model(|cx| Buffer::local("ABCDEFGHIJKLMNOP", cx));
    let multibuffer = cx.new_model(|_| MultiBuffer::new(Capability::ReadWrite));

    // Create an insertion id in buffer 1 that doesn't exist in buffer 2.
    // Add an excerpt from buffer 1 that spans this new insertion.
    buffer_1.update(cx, |buffer, cx| buffer.edit([(4..4, "123")], None, cx));
    let excerpt_id_1 = multibuffer.update(cx, |multibuffer, cx| {
        multibuffer
            .push_excerpts(
                buffer_1.clone(),
                [ExcerptRange {
                    context: 0..7,
                    primary: None,
                }],
                cx,
            )
            .pop()
            .unwrap()
    });

    let snapshot_1 = multibuffer.read(cx).snapshot(cx);
    assert_eq!(snapshot_1.text(), "abcd123");

    // Replace the buffer 1 excerpt with new excerpts from buffer 2.
    let (excerpt_id_2, excerpt_id_3) = multibuffer.update(cx, |multibuffer, cx| {
        multibuffer.remove_excerpts([excerpt_id_1], cx);
        let mut ids = multibuffer
            .push_excerpts(
                buffer_2.clone(),
                [
                    ExcerptRange {
                        context: 0..4,
                        primary: None,
                    },
                    ExcerptRange {
                        context: 6..10,
                        primary: None,
                    },
                    ExcerptRange {
                        context: 12..16,
                        primary: None,
                    },
                ],
                cx,
            )
            .into_iter();
        (ids.next().unwrap(), ids.next().unwrap())
    });
    let snapshot_2 = multibuffer.read(cx).snapshot(cx);
    assert_eq!(snapshot_2.text(), "ABCD\nGHIJ\nMNOP");

    // The old excerpt id doesn't get reused.
    assert_ne!(excerpt_id_2, excerpt_id_1);

    // Resolve some anchors from the previous snapshot in the new snapshot.
    // The current excerpts are from a different buffer, so we don't attempt to
    // resolve the old text anchor in the new buffer.
    assert_eq!(
        snapshot_2.summary_for_anchor::<usize>(&snapshot_1.anchor_before(2)),
        0
    );
    assert_eq!(
        snapshot_2.summaries_for_anchors::<usize, _>(&[
            snapshot_1.anchor_before(2),
            snapshot_1.anchor_after(3)
        ]),
        vec![0, 0]
    );

    // Refresh anchors from the old snapshot. The return value indicates that both
    // anchors lost their original excerpt.
    let refresh =
        snapshot_2.refresh_anchors(&[snapshot_1.anchor_before(2), snapshot_1.anchor_after(3)]);
    assert_eq!(
        refresh,
        &[
            (0, snapshot_2.anchor_before(0), false),
            (1, snapshot_2.anchor_after(0), false),
        ]
    );

    // Replace the middle excerpt with a smaller excerpt in buffer 2,
    // that intersects the old excerpt.
    let excerpt_id_5 = multibuffer.update(cx, |multibuffer, cx| {
        multibuffer.remove_excerpts([excerpt_id_3], cx);
        multibuffer
            .insert_excerpts_after(
                excerpt_id_2,
                buffer_2.clone(),
                [ExcerptRange {
                    context: 5..8,
                    primary: None,
                }],
                cx,
            )
            .pop()
            .unwrap()
    });

    let snapshot_3 = multibuffer.read(cx).snapshot(cx);
    assert_eq!(snapshot_3.text(), "ABCD\nFGH\nMNOP");
    assert_ne!(excerpt_id_5, excerpt_id_3);

    // Resolve some anchors from the previous snapshot in the new snapshot.
    // The third anchor can't be resolved, since its excerpt has been removed,
    // so it resolves to the same position as its predecessor.
    let anchors = [
        snapshot_2.anchor_before(0),
        snapshot_2.anchor_after(2),
        snapshot_2.anchor_after(6),
        snapshot_2.anchor_after(14),
    ];
    assert_eq!(
        snapshot_3.summaries_for_anchors::<usize, _>(&anchors),
        &[0, 2, 9, 13]
    );

    let new_anchors = snapshot_3.refresh_anchors(&anchors);
    assert_eq!(
        new_anchors.iter().map(|a| (a.0, a.2)).collect::<Vec<_>>(),
        &[(0, true), (1, true), (2, true), (3, true)]
    );
    assert_eq!(
        snapshot_3.summaries_for_anchors::<usize, _>(new_anchors.iter().map(|a| &a.1)),
        &[0, 2, 7, 13]
    );
}

#[gpui::test]
fn test_basic_diff_hunks(cx: &mut TestAppContext) {
    let text = indoc!(
        "
        ZERO
        one
        TWO
        three
        six
        "
    );
    let base_text = indoc!(
        "
        one
        two
        three
        four
        five
        six
        "
    );

    let buffer = cx.new_model(|cx| Buffer::local(text, cx));
    let change_set =
        cx.new_model(|cx| BufferChangeSet::new_with_base_text(base_text.to_string(), &buffer, cx));
    cx.run_until_parked();

    let multibuffer = cx.new_model(|cx| {
        let mut multibuffer = MultiBuffer::singleton(buffer.clone(), cx);
        multibuffer.add_change_set(change_set.clone(), cx);
        multibuffer
    });

    let (mut snapshot, mut subscription) = multibuffer.update(cx, |multibuffer, cx| {
        (multibuffer.snapshot(cx), multibuffer.subscribe())
    });
    assert_eq!(
        snapshot.text(),
        indoc!(
            "
            ZERO
            one
            TWO
            three
            six
            "
        ),
    );

    multibuffer.update(cx, |multibuffer, cx| {
        multibuffer.expand_diff_hunks(vec![Anchor::min()..Anchor::max()], cx);
    });

    assert_new_snapshot(
        &multibuffer,
        &mut snapshot,
        &mut subscription,
        cx,
        indoc!(
            "
            + ZERO
              one
            - two
            + TWO
              three
            - four
            - five
              six
            "
        ),
    );

    assert_eq!(
        snapshot
            .row_infos(MultiBufferRow(0))
            .map(|info| info.buffer_row)
            .collect::<Vec<_>>(),
        vec![
            Some(0),
            Some(1),
            None,
            Some(2),
            Some(3),
            None,
            None,
            Some(4),
            Some(5)
        ]
    );

    assert_chunks_in_ranges(&snapshot);
    assert_consistent_line_numbers(&snapshot);
    assert_position_translation(&snapshot);

    multibuffer.update(cx, |multibuffer, cx| {
        multibuffer.collapse_diff_hunks(vec![Anchor::min()..Anchor::max()], cx)
    });
    assert_new_snapshot(
        &multibuffer,
        &mut snapshot,
        &mut subscription,
        cx,
        indoc!(
            "
            ZERO
            one
            TWO
            three
            six
            "
        ),
    );

    assert_chunks_in_ranges(&snapshot);
    assert_consistent_line_numbers(&snapshot);
    assert_position_translation(&snapshot);

    // Expand the first diff hunk
    multibuffer.update(cx, |multibuffer, cx| {
        let position = multibuffer.read(cx).anchor_before(Point::new(2, 2));
        multibuffer.expand_diff_hunks(vec![position..position], cx)
    });
    assert_new_snapshot(
        &multibuffer,
        &mut snapshot,
        &mut subscription,
        cx,
        indoc!(
            "
              ZERO
              one
            - two
            + TWO
              three
              six
            "
        ),
    );

    // Expand the second diff hunk
    multibuffer.update(cx, |multibuffer, cx| {
        let start = multibuffer.read(cx).anchor_before(Point::new(4, 0));
        let end = multibuffer.read(cx).anchor_before(Point::new(5, 0));
        multibuffer.expand_diff_hunks(vec![start..end], cx)
    });
    assert_new_snapshot(
        &multibuffer,
        &mut snapshot,
        &mut subscription,
        cx,
        indoc!(
            "
              ZERO
              one
            - two
            + TWO
              three
            - four
            - five
              six
            "
        ),
    );

    assert_chunks_in_ranges(&snapshot);
    assert_consistent_line_numbers(&snapshot);
    assert_position_translation(&snapshot);

    // Edit the buffer before the first hunk
    buffer.update(cx, |buffer, cx| {
        buffer.edit_via_marked_text(
            indoc!(
                "
                ZERO
                one« hundred
                  thousand»
                TWO
                three
                six
                "
            ),
            None,
            cx,
        );
    });
    assert_new_snapshot(
        &multibuffer,
        &mut snapshot,
        &mut subscription,
        cx,
        indoc!(
            "
              ZERO
              one hundred
                thousand
            - two
            + TWO
              three
            - four
            - five
              six
            "
        ),
    );

    assert_chunks_in_ranges(&snapshot);
    assert_consistent_line_numbers(&snapshot);
    assert_position_translation(&snapshot);

    // Recalculate the diff, changing the first diff hunk.
    let _ = change_set.update(cx, |change_set, cx| {
        change_set.recalculate_diff(buffer.read(cx).text_snapshot(), cx)
    });
    cx.run_until_parked();
    assert_new_snapshot(
        &multibuffer,
        &mut snapshot,
        &mut subscription,
        cx,
        indoc!(
            "
              ZERO
              one hundred
                thousand
              TWO
              three
            - four
            - five
              six
            "
        ),
    );

    assert_eq!(
        snapshot
            .diff_hunks_in_range(0..snapshot.len())
            .map(|hunk| hunk.row_range.start.0..hunk.row_range.end.0)
            .collect::<Vec<_>>(),
        &[0..4, 5..7]
    );
}

#[gpui::test]
fn test_diff_hunks_with_multiple_excerpts(cx: &mut TestAppContext) {
    let base_text_1 = indoc!(
        "
        one
        two
        three
        four
        five
        six
        "
    );
    let text_1 = indoc!(
        "
        ZERO
        one
        TWO
        three
        six
        "
    );
    let base_text_2 = indoc!(
        "
        seven
        eight
        nine
        ten
        eleven
        twelve
        "
    );
    let text_2 = indoc!(
        "
        eight
        nine
        eleven
        THIRTEEN
        FOURTEEN
        "
    );

    let buffer_1 = cx.new_model(|cx| Buffer::local(text_1, cx));
    let buffer_2 = cx.new_model(|cx| Buffer::local(text_2, cx));
    let change_set_1 = cx.new_model(|cx| {
        BufferChangeSet::new_with_base_text(base_text_1.to_string(), &buffer_1, cx)
    });
    let change_set_2 = cx.new_model(|cx| {
        BufferChangeSet::new_with_base_text(base_text_2.to_string(), &buffer_2, cx)
    });
    cx.run_until_parked();

    let multibuffer = cx.new_model(|cx| {
        let mut multibuffer = MultiBuffer::new(Capability::ReadWrite);
        multibuffer.push_excerpts(
            buffer_1.clone(),
            [ExcerptRange {
                context: text::Anchor::MIN..text::Anchor::MAX,
                primary: None,
            }],
            cx,
        );
        multibuffer.push_excerpts(
            buffer_2.clone(),
            [ExcerptRange {
                context: text::Anchor::MIN..text::Anchor::MAX,
                primary: None,
            }],
            cx,
        );
        multibuffer.add_change_set(change_set_1.clone(), cx);
        multibuffer.add_change_set(change_set_2.clone(), cx);
        multibuffer
    });

    let (mut snapshot, mut subscription) = multibuffer.update(cx, |multibuffer, cx| {
        (multibuffer.snapshot(cx), multibuffer.subscribe())
    });
    assert_eq!(
        snapshot.text(),
        indoc!(
            "
            ZERO
            one
            TWO
            three
            six

            eight
            nine
            eleven
            THIRTEEN
            FOURTEEN
            "
        ),
    );

    multibuffer.update(cx, |multibuffer, cx| {
        multibuffer.expand_diff_hunks(vec![Anchor::min()..Anchor::max()], cx);
    });

    assert_new_snapshot(
        &multibuffer,
        &mut snapshot,
        &mut subscription,
        cx,
        indoc!(
            "
            + ZERO
              one
            - two
            + TWO
              three
            - four
            - five
              six

            - seven
              eight
              nine
            - ten
              eleven
            - twelve
            + THIRTEEN
            + FOURTEEN
            "
        ),
    );

    let id_1 = buffer_1.read_with(cx, |buffer, _| buffer.remote_id());
    let id_2 = buffer_2.read_with(cx, |buffer, _| buffer.remote_id());
    let base_id_1 = change_set_1.read_with(cx, |change_set, _| {
        change_set.base_text.as_ref().unwrap().remote_id()
    });
    let base_id_2 = change_set_2.read_with(cx, |change_set, _| {
        change_set.base_text.as_ref().unwrap().remote_id()
    });

    let buffer_lines = (0..=snapshot.max_row().0)
        .map(|row| {
            let (buffer, range) = snapshot.buffer_line_for_row(MultiBufferRow(row))?;
            Some((
                buffer.remote_id(),
                buffer.text_for_range(range).collect::<String>(),
            ))
        })
        .collect::<Vec<_>>();
    pretty_assertions::assert_eq!(
        buffer_lines,
        [
            Some((id_1, "ZERO".into())),
            Some((id_1, "one".into())),
            Some((base_id_1, "two".into())),
            Some((id_1, "TWO".into())),
            Some((id_1, "three".into())),
            Some((base_id_1, "four".into())),
            Some((base_id_1, "five".into())),
            Some((id_1, "six".into())),
            Some((id_1, "".into())),
            Some((base_id_2, "seven".into())),
            Some((id_2, "eight".into())),
            Some((id_2, "nine".into())),
            Some((base_id_2, "ten".into())),
            Some((id_2, "eleven".into())),
            Some((base_id_2, "twelve".into())),
            Some((id_2, "THIRTEEN".into())),
            Some((id_2, "FOURTEEN".into())),
            Some((id_2, "".into())),
        ]
    );

    assert_position_translation(&snapshot);

    assert_eq!(
        snapshot
            .diff_hunks_in_range(0..snapshot.len())
            .map(|hunk| hunk.row_range.start.0..hunk.row_range.end.0)
            .collect::<Vec<_>>(),
        &[0..1, 2..4, 5..7, 9..10, 12..13, 14..17]
    );

    buffer_2.update(cx, |buffer, cx| {
        buffer.edit_via_marked_text(
            indoc!(
                "
                eight
                «»eleven
                THIRTEEN
                FOURTEEN
                "
            ),
            None,
            cx,
        );
    });

    assert_new_snapshot(
        &multibuffer,
        &mut snapshot,
        &mut subscription,
        cx,
        indoc!(
            "
            + ZERO
              one
            - two
            + TWO
              three
            - four
            - five
              six

            - seven
              eight
              eleven
            - twelve
            + THIRTEEN
            + FOURTEEN
            "
        ),
    );
}

/// A naive implementation of a multi-buffer that does not maintain
/// any derived state, used for comparison in a randomized test.
#[derive(Default)]
struct ReferenceMultibuffer {
    excerpts: Vec<ReferenceExcerpt>,
    change_sets: HashMap<BufferId, Model<BufferChangeSet>>,
}

struct ReferenceExcerpt {
    id: ExcerptId,
    buffer: Model<Buffer>,
    range: Range<text::Anchor>,
    expanded_diff_hunks: Vec<text::Anchor>,
}

impl ReferenceMultibuffer {
    fn expand_excerpts(&mut self, excerpts: &HashSet<ExcerptId>, line_count: u32, cx: &AppContext) {
        if line_count == 0 {
            return;
        }

        for id in excerpts {
            let excerpt = self.excerpts.iter_mut().find(|e| e.id == *id).unwrap();
            let snapshot = excerpt.buffer.read(cx).snapshot();
            let mut point_range = excerpt.range.to_point(&snapshot);
            point_range.start = Point::new(point_range.start.row.saturating_sub(line_count), 0);
            point_range.end =
                snapshot.clip_point(Point::new(point_range.end.row + line_count, 0), Bias::Left);
            point_range.end.column = snapshot.line_len(point_range.end.row);
            excerpt.range =
                snapshot.anchor_before(point_range.start)..snapshot.anchor_after(point_range.end);
        }
    }

    fn remove_excerpt(&mut self, id: ExcerptId, cx: &AppContext) {
        let ix = self
            .excerpts
            .iter()
            .position(|excerpt| excerpt.id == id)
            .unwrap();
        let excerpt = self.excerpts.remove(ix);
        let buffer = excerpt.buffer.read(cx);
        log::info!(
            "Removing excerpt {}: {:?}",
            ix,
            buffer
                .text_for_range(excerpt.range.to_offset(buffer))
                .collect::<String>(),
        );
    }

    fn insert_excerpt_after(
        &mut self,
        prev_id: ExcerptId,
        new_excerpt_id: ExcerptId,
        (buffer_handle, anchor_range): (Model<Buffer>, Range<text::Anchor>),
    ) {
        let excerpt_ix = if prev_id == ExcerptId::max() {
            self.excerpts.len()
        } else {
            self.excerpts
                .iter()
                .position(|excerpt| excerpt.id == prev_id)
                .unwrap()
                + 1
        };
        self.excerpts.insert(
            excerpt_ix,
            ReferenceExcerpt {
                id: new_excerpt_id,
                buffer: buffer_handle,
                range: anchor_range,
                expanded_diff_hunks: Vec::new(),
            },
        );
    }

    fn expand_diff_hunks(
        &mut self,
        excerpt_id: ExcerptId,
        range: Range<text::Anchor>,
        cx: &AppContext,
    ) {
        let excerpt = self
            .excerpts
            .iter_mut()
            .find(|e| e.id == excerpt_id)
            .unwrap();
        let buffer = excerpt.buffer.read(cx).snapshot();
        let buffer_id = buffer.remote_id();
        let Some(change_set) = self.change_sets.get(&buffer_id) else {
            return;
        };
        let diff = change_set.read(cx).diff_to_buffer.clone();
        for hunk in diff.hunks_intersecting_range(range, &buffer) {
            if let Err(ix) = excerpt
                .expanded_diff_hunks
                .binary_search_by(|anchor| anchor.cmp(&hunk.buffer_range.start, &buffer))
            {
                log::info!(
                    "expanding diff hunk {:?}",
                    hunk.buffer_range.to_offset(&buffer)
                );
                excerpt
                    .expanded_diff_hunks
                    .insert(ix, hunk.buffer_range.start);
            }
        }
    }

    fn expected_content(&self, cx: &AppContext) -> (String, Vec<RowInfo>) {
        let mut expected_text = String::new();
        let mut expected_buffer_rows = Vec::new();
        for excerpt in &self.excerpts {
            let buffer = excerpt.buffer.read(cx);
            let buffer_range = excerpt.range.to_offset(buffer);
            let change_set = self.change_sets.get(&buffer.remote_id()).unwrap().read(cx);
            let diff = change_set.diff_to_buffer.clone();
            let base_buffer = change_set.base_text.as_ref().unwrap();

            let mut start = buffer_range.start;
            let mut insertion_end_point = Point::zero();

            for hunk in diff.hunks_intersecting_range(excerpt.range.clone(), buffer) {
                if !excerpt
                    .expanded_diff_hunks
                    .contains(&hunk.buffer_range.start)
                {
                    continue;
                }

                let hunk_offset = hunk.buffer_range.start.to_offset(buffer);
                if hunk_offset > start {
                    // Add the buffer text before the hunk
                    expected_text.extend(buffer.text_for_range(start..hunk_offset));
                    let start_point = buffer.offset_to_point(start);
                    let end_point = buffer.offset_to_point(hunk_offset);
                    for row in start_point.row..end_point.row {
                        expected_buffer_rows.push(RowInfo {
                            buffer_row: Some(row),
                            diff_status: if Point::new(row, 0) < insertion_end_point {
                                Some(DiffHunkStatus::Added)
                            } else {
                                None
                            },
                        });
                    }

                    // Add the deleted text for the hunk.
                    if !hunk.diff_base_byte_range.is_empty() {
                        let mut base_text = base_buffer
                            .text_for_range(hunk.diff_base_byte_range)
                            .collect::<String>();
                        if !base_text.ends_with('\n') {
                            base_text.push('\n');
                        }
                        expected_text.push_str(&base_text);
                        for _ in base_text.matches('\n') {
                            expected_buffer_rows.push(RowInfo {
                                buffer_row: None,
                                diff_status: Some(DiffHunkStatus::Removed),
                            });
                        }
                    }

                    start = hunk_offset;
                }

                insertion_end_point = buffer.offset_to_point(
                    hunk.buffer_range
                        .end
                        .to_offset(&buffer)
                        .min(buffer_range.end),
                );
            }

            expected_text.extend(buffer.text_for_range(start..buffer_range.end));
            expected_text.push('\n');
            let buffer_row_range =
                buffer.offset_to_point(start).row..=buffer.offset_to_point(buffer_range.end).row;
            for row in buffer_row_range {
                expected_buffer_rows.push(RowInfo {
                    buffer_row: Some(row),
                    diff_status: if Point::new(row, 0) < insertion_end_point {
                        Some(DiffHunkStatus::Added)
                    } else {
                        None
                    },
                });
            }
        }
        // Remove final trailing newline.
        if !self.excerpts.is_empty() {
            expected_text.pop();
        }

        // Always report one buffer row
        if expected_buffer_rows.is_empty() {
            expected_buffer_rows.push(RowInfo {
                buffer_row: Some(0),
                diff_status: None,
            });
        }

        (expected_text, expected_buffer_rows)
    }

    fn diffs_updated(&mut self, cx: &AppContext) {
        for excerpt in &mut self.excerpts {
            let buffer = excerpt.buffer.read(cx).snapshot();
            let buffer_id = buffer.remote_id();
            let diff = &self
                .change_sets
                .get(&buffer_id)
                .unwrap()
                .read(cx)
                .diff_to_buffer;
            let mut hunks = diff.hunks_in_row_range(0..u32::MAX, &buffer).peekable();
            excerpt.expanded_diff_hunks.retain(|hunk_anchor| {
                while let Some(hunk) = hunks.peek() {
                    match hunk.buffer_range.start.cmp(&hunk_anchor, &buffer) {
                        cmp::Ordering::Less => {
                            hunks.next();
                        }
                        cmp::Ordering::Equal => return true,
                        cmp::Ordering::Greater => break,
                    }
                }
                false
            });
        }
    }

    fn add_change_set(&mut self, change_set: Model<BufferChangeSet>, cx: &mut AppContext) {
        let buffer_id = change_set.read(cx).buffer_id;
        self.change_sets.insert(buffer_id, change_set);
    }
}

#[gpui::test(iterations = 100)]
fn test_random_multibuffer(cx: &mut AppContext, mut rng: StdRng) {
    let operations = env::var("OPERATIONS")
        .map(|i| i.parse().expect("invalid `OPERATIONS` variable"))
        .unwrap_or(10);

    let mut buffers: Vec<Model<Buffer>> = Vec::new();
    let multibuffer = cx.new_model(|_| MultiBuffer::new(Capability::ReadWrite));
    let mut reference = ReferenceMultibuffer::default();
    let mut anchors = Vec::new();
    let mut old_versions = Vec::new();

    for _ in 0..operations {
        match rng.gen_range(0..100) {
            0..=14 if !buffers.is_empty() => {
                let buffer = buffers.choose(&mut rng).unwrap();
                buffer.update(cx, |buf, cx| {
                    let edit_count = rng.gen_range(1..5);
                    buf.randomly_edit(&mut rng, edit_count, cx)
                });
            }
            15..=19 if !reference.excerpts.is_empty() => {
                multibuffer.update(cx, |multibuffer, cx| {
                    let ids = multibuffer.excerpt_ids();
                    let mut excerpts = HashSet::default();
                    for _ in 0..rng.gen_range(0..ids.len()) {
                        excerpts.extend(ids.choose(&mut rng).copied());
                    }

                    let line_count = rng.gen_range(0..5);

                    let excerpt_ixs = excerpts
                        .iter()
                        .map(|id| reference.excerpts.iter().position(|e| e.id == *id).unwrap())
                        .collect::<Vec<_>>();
                    log::info!("Expanding excerpts {excerpt_ixs:?} by {line_count} lines");
                    multibuffer.expand_excerpts(
                        excerpts.iter().cloned(),
                        line_count,
                        ExpandExcerptDirection::UpAndDown,
                        cx,
                    );

                    reference.expand_excerpts(&excerpts, line_count, cx);
                });
            }
            20..=29 if !reference.excerpts.is_empty() => {
                let mut ids_to_remove = vec![];
                for _ in 0..rng.gen_range(1..=3) {
                    let Some(excerpt) = reference.excerpts.choose(&mut rng) else {
                        break;
                    };
                    let id = excerpt.id;
                    reference.remove_excerpt(id, cx);
                    ids_to_remove.push(id);
                }
                let snapshot = multibuffer.read(cx).read(cx);
                ids_to_remove.sort_unstable_by(|a, b| a.cmp(b, &snapshot));
                drop(snapshot);
                multibuffer.update(cx, |multibuffer, cx| {
                    multibuffer.remove_excerpts(ids_to_remove, cx)
                });
            }
            30..=39 if !reference.excerpts.is_empty() => {
                let multibuffer = multibuffer.read(cx).read(cx);
                let offset =
                    multibuffer.clip_offset(rng.gen_range(0..=multibuffer.len()), Bias::Left);
                let bias = if rng.gen() { Bias::Left } else { Bias::Right };
                log::info!("Creating anchor at {} with bias {:?}", offset, bias);
                anchors.push(multibuffer.anchor_at(offset, bias));
                anchors.sort_by(|a, b| a.cmp(b, &multibuffer));
            }
            40..=44 if !anchors.is_empty() => {
                let multibuffer = multibuffer.read(cx).read(cx);
                let prev_len = anchors.len();
                anchors = multibuffer
                    .refresh_anchors(&anchors)
                    .into_iter()
                    .map(|a| a.1)
                    .collect();

                // Ensure the newly-refreshed anchors point to a valid excerpt and don't
                // overshoot its boundaries.
                assert_eq!(anchors.len(), prev_len);
                for anchor in &anchors {
                    if anchor.excerpt_id == ExcerptId::min()
                        || anchor.excerpt_id == ExcerptId::max()
                    {
                        continue;
                    }

                    let excerpt = multibuffer.excerpt(anchor.excerpt_id).unwrap();
                    assert_eq!(excerpt.id, anchor.excerpt_id);
                    assert!(excerpt.contains(anchor));
                }
            }
            45..=55 if !reference.excerpts.is_empty() => {
                multibuffer.update(cx, |multibuffer, cx| {
                    let snapshot = multibuffer.snapshot(cx);
                    let excerpt_ix = rng.gen_range(0..reference.excerpts.len());
                    let excerpt = &reference.excerpts[excerpt_ix];
                    let start = excerpt.range.start;
                    let end = excerpt.range.end;
                    let range = snapshot.anchor_in_excerpt(excerpt.id, start).unwrap()
                        ..snapshot.anchor_in_excerpt(excerpt.id, end).unwrap();

                    multibuffer.expand_diff_hunks(vec![range], cx);
                    reference.expand_diff_hunks(excerpt.id, start..end, cx);
                });
            }
            56..=65 => {
                multibuffer.update(cx, |multibuffer, cx| {
                    for buffer in multibuffer.all_buffers() {
                        let snapshot = buffer.read(cx).snapshot();
                        let _ = multibuffer
                            .change_set_for(snapshot.remote_id())
                            .unwrap()
                            .update(cx, |change_set, cx| {
                                log::info!(
                                    "recalculating diff for buffer {:?}",
                                    snapshot.remote_id(),
                                );
                                change_set.recalculate_diff_sync(
                                    change_set.base_text.clone().unwrap().text(),
                                    snapshot.text,
                                    false,
                                    cx,
                                )
                            });
                    }
                    reference.diffs_updated(cx);
                });
            }
            _ => {
                let buffer_handle = if buffers.is_empty() || rng.gen_bool(0.4) {
                    let base_text = util::RandomCharIter::new(&mut rng)
                        .take(256)
                        .collect::<String>();

                    let buffer = cx.new_model(|cx| Buffer::local(base_text.clone(), cx));
                    let snapshot = buffer.read(cx).snapshot();
                    let change_set = cx.new_model(|cx| {
                        let mut change_set = BufferChangeSet::new(&buffer, cx);
                        change_set.recalculate_diff_sync(base_text, snapshot.text, true, cx);
                        change_set
                    });

                    reference.add_change_set(change_set.clone(), cx);
                    multibuffer.update(cx, |multibuffer, cx| {
                        multibuffer.add_change_set(change_set, cx)
                    });
                    buffers.push(buffer);
                    buffers.last().unwrap()
                } else {
                    buffers.choose(&mut rng).unwrap()
                };

                let buffer = buffer_handle.read(cx);
                let end_row = rng.gen_range(0..=buffer.max_point().row);
                let start_row = rng.gen_range(0..=end_row);
                let end_ix = buffer.point_to_offset(Point::new(end_row, 0));
                let start_ix = buffer.point_to_offset(Point::new(start_row, 0));
                let anchor_range = buffer.anchor_before(start_ix)..buffer.anchor_after(end_ix);
                let prev_excerpt_ix = rng.gen_range(0..=reference.excerpts.len());
                let prev_excerpt_id = reference
                    .excerpts
                    .get(prev_excerpt_ix)
                    .map_or(ExcerptId::max(), |e| e.id);
                let excerpt_ix = (prev_excerpt_ix + 1).min(reference.excerpts.len());

                log::info!(
                    "Inserting excerpt at {} of {} for buffer {}: {:?}[{:?}] = {:?}",
                    excerpt_ix,
                    reference.excerpts.len(),
                    buffer_handle.read(cx).remote_id(),
                    buffer.text(),
                    start_ix..end_ix,
                    &buffer.text()[start_ix..end_ix]
                );

                let excerpt_id = multibuffer.update(cx, |multibuffer, cx| {
                    multibuffer
                        .insert_excerpts_after(
                            prev_excerpt_id,
                            buffer_handle.clone(),
                            [ExcerptRange {
                                context: start_ix..end_ix,
                                primary: None,
                            }],
                            cx,
                        )
                        .pop()
                        .unwrap()
                });

                reference.insert_excerpt_after(
                    prev_excerpt_id,
                    excerpt_id,
                    (buffer_handle.clone(), anchor_range),
                );
            }
        }

        if rng.gen_bool(0.3) {
            multibuffer.update(cx, |multibuffer, cx| {
                old_versions.push((multibuffer.snapshot(cx), multibuffer.subscribe()));
            })
        }

        let snapshot = multibuffer.read(cx).snapshot(cx);
        let actual_text = snapshot.text();
        let actual_row_infos = snapshot.row_infos(MultiBufferRow(0)).collect::<Vec<_>>();
        let actual_diff = format_diff(&actual_text, &actual_row_infos);

        let (expected_text, expected_row_infos) = reference.expected_content(cx);
        let expected_diff = format_diff(&expected_text, &expected_row_infos);

        log::info!("Multibuffer content:\n{}", actual_diff);

        pretty_assertions::assert_eq!(actual_diff, expected_diff);
        pretty_assertions::assert_eq!(actual_text, expected_text);
        pretty_assertions::assert_eq!(actual_row_infos, expected_row_infos);

        for _ in 0..5 {
            let start_row = rng.gen_range(0..=expected_row_infos.len());
            assert_eq!(
                snapshot
                    .row_infos(MultiBufferRow(start_row as u32))
                    .collect::<Vec<_>>(),
                &expected_row_infos[start_row..],
                "buffer_rows({})",
                start_row
            );
        }

        assert_eq!(
            snapshot.widest_line_number(),
            expected_row_infos
                .into_iter()
                .map(|info| info.buffer_row)
                .flatten()
                .max()
                .unwrap()
                + 1
        );

        assert_position_translation(&snapshot);

        for (row, line) in expected_text.split('\n').enumerate() {
            assert_eq!(
                snapshot.line_len(MultiBufferRow(row as u32)),
                line.len() as u32,
                "line_len({}).",
                row
            );
        }

        let text_rope = Rope::from(expected_text.as_str());
        for _ in 0..10 {
            let end_ix = text_rope.clip_offset(rng.gen_range(0..=text_rope.len()), Bias::Right);
            let start_ix = text_rope.clip_offset(rng.gen_range(0..=end_ix), Bias::Left);

            let text_for_range = snapshot
                .text_for_range(start_ix..end_ix)
                .collect::<String>();
            assert_eq!(
                text_for_range,
                &expected_text[start_ix..end_ix],
                "incorrect text for range {:?}",
                start_ix..end_ix
            );

            let expected_summary = TextSummary::from(&expected_text[start_ix..end_ix]);
            assert_eq!(
                snapshot.text_summary_for_range::<TextSummary, _>(start_ix..end_ix),
                expected_summary,
                "incorrect summary for range {:?}",
                start_ix..end_ix
            );
        }

        // Anchor resolution
        let summaries = snapshot.summaries_for_anchors::<usize, _>(&anchors);
        assert_eq!(anchors.len(), summaries.len());
        for (anchor, resolved_offset) in anchors.iter().zip(summaries) {
            assert!(resolved_offset <= snapshot.len());
            assert_eq!(
                snapshot.summary_for_anchor::<usize>(anchor),
                resolved_offset
            );
        }

        for _ in 0..10 {
            let end_ix = text_rope.clip_offset(rng.gen_range(0..=text_rope.len()), Bias::Right);
            assert_eq!(
                snapshot.reversed_chars_at(end_ix).collect::<String>(),
                expected_text[..end_ix].chars().rev().collect::<String>(),
            );
        }

        for _ in 0..10 {
            let end_ix = rng.gen_range(0..=text_rope.len());
            let start_ix = rng.gen_range(0..=end_ix);
            assert_eq!(
                snapshot
                    .bytes_in_range(start_ix..end_ix)
                    .flatten()
                    .copied()
                    .collect::<Vec<_>>(),
                expected_text.as_bytes()[start_ix..end_ix].to_vec(),
                "bytes_in_range({:?})",
                start_ix..end_ix,
            );
        }
    }

    let snapshot = multibuffer.read(cx).snapshot(cx);
    for (old_snapshot, subscription) in old_versions {
        let edits = subscription.consume().into_inner();

        log::info!(
            "applying subscription edits to old text: {:?}: {:?}",
            old_snapshot.text(),
            edits,
        );

        let mut text = old_snapshot.text();
        for edit in edits {
            let new_text: String = snapshot.text_for_range(edit.new.clone()).collect();
            text.replace_range(edit.new.start..edit.new.start + edit.old.len(), &new_text);
        }
        assert_eq!(text.to_string(), snapshot.text());
    }
}

#[gpui::test]
fn test_history(cx: &mut AppContext) {
    let test_settings = SettingsStore::test(cx);
    cx.set_global(test_settings);
    let group_interval: Duration = Duration::from_millis(1);
    let buffer_1 = cx.new_model(|cx| {
        let mut buf = Buffer::local("1234", cx);
        buf.set_group_interval(group_interval);
        buf
    });
    let buffer_2 = cx.new_model(|cx| {
        let mut buf = Buffer::local("5678", cx);
        buf.set_group_interval(group_interval);
        buf
    });
    let multibuffer = cx.new_model(|_| MultiBuffer::new(Capability::ReadWrite));
    multibuffer.update(cx, |this, _| {
        this.history.group_interval = group_interval;
    });
    multibuffer.update(cx, |multibuffer, cx| {
        multibuffer.push_excerpts(
            buffer_1.clone(),
            [ExcerptRange {
                context: 0..buffer_1.read(cx).len(),
                primary: None,
            }],
            cx,
        );
        multibuffer.push_excerpts(
            buffer_2.clone(),
            [ExcerptRange {
                context: 0..buffer_2.read(cx).len(),
                primary: None,
            }],
            cx,
        );
    });

    let mut now = Instant::now();

    multibuffer.update(cx, |multibuffer, cx| {
        let transaction_1 = multibuffer.start_transaction_at(now, cx).unwrap();
        multibuffer.edit(
            [
                (Point::new(0, 0)..Point::new(0, 0), "A"),
                (Point::new(1, 0)..Point::new(1, 0), "A"),
            ],
            None,
            cx,
        );
        multibuffer.edit(
            [
                (Point::new(0, 1)..Point::new(0, 1), "B"),
                (Point::new(1, 1)..Point::new(1, 1), "B"),
            ],
            None,
            cx,
        );
        multibuffer.end_transaction_at(now, cx);
        assert_eq!(multibuffer.read(cx).text(), "AB1234\nAB5678");

        // Verify edited ranges for transaction 1
        assert_eq!(
            multibuffer.edited_ranges_for_transaction(transaction_1, cx),
            &[
                Point::new(0, 0)..Point::new(0, 2),
                Point::new(1, 0)..Point::new(1, 2)
            ]
        );

        // Edit buffer 1 through the multibuffer
        now += 2 * group_interval;
        multibuffer.start_transaction_at(now, cx);
        multibuffer.edit([(2..2, "C")], None, cx);
        multibuffer.end_transaction_at(now, cx);
        assert_eq!(multibuffer.read(cx).text(), "ABC1234\nAB5678");

        // Edit buffer 1 independently
        buffer_1.update(cx, |buffer_1, cx| {
            buffer_1.start_transaction_at(now);
            buffer_1.edit([(3..3, "D")], None, cx);
            buffer_1.end_transaction_at(now, cx);

            now += 2 * group_interval;
            buffer_1.start_transaction_at(now);
            buffer_1.edit([(4..4, "E")], None, cx);
            buffer_1.end_transaction_at(now, cx);
        });
        assert_eq!(multibuffer.read(cx).text(), "ABCDE1234\nAB5678");

        // An undo in the multibuffer undoes the multibuffer transaction
        // and also any individual buffer edits that have occurred since
        // that transaction.
        multibuffer.undo(cx);
        assert_eq!(multibuffer.read(cx).text(), "AB1234\nAB5678");

        multibuffer.undo(cx);
        assert_eq!(multibuffer.read(cx).text(), "1234\n5678");

        multibuffer.redo(cx);
        assert_eq!(multibuffer.read(cx).text(), "AB1234\nAB5678");

        multibuffer.redo(cx);
        assert_eq!(multibuffer.read(cx).text(), "ABCDE1234\nAB5678");

        // Undo buffer 2 independently.
        buffer_2.update(cx, |buffer_2, cx| buffer_2.undo(cx));
        assert_eq!(multibuffer.read(cx).text(), "ABCDE1234\n5678");

        // An undo in the multibuffer undoes the components of the
        // the last multibuffer transaction that are not already undone.
        multibuffer.undo(cx);
        assert_eq!(multibuffer.read(cx).text(), "AB1234\n5678");

        multibuffer.undo(cx);
        assert_eq!(multibuffer.read(cx).text(), "1234\n5678");

        multibuffer.redo(cx);
        assert_eq!(multibuffer.read(cx).text(), "AB1234\nAB5678");

        buffer_1.update(cx, |buffer_1, cx| buffer_1.redo(cx));
        assert_eq!(multibuffer.read(cx).text(), "ABCD1234\nAB5678");

        // Redo stack gets cleared after an edit.
        now += 2 * group_interval;
        multibuffer.start_transaction_at(now, cx);
        multibuffer.edit([(0..0, "X")], None, cx);
        multibuffer.end_transaction_at(now, cx);
        assert_eq!(multibuffer.read(cx).text(), "XABCD1234\nAB5678");
        multibuffer.redo(cx);
        assert_eq!(multibuffer.read(cx).text(), "XABCD1234\nAB5678");
        multibuffer.undo(cx);
        assert_eq!(multibuffer.read(cx).text(), "ABCD1234\nAB5678");
        multibuffer.undo(cx);
        assert_eq!(multibuffer.read(cx).text(), "1234\n5678");

        // Transactions can be grouped manually.
        multibuffer.redo(cx);
        multibuffer.redo(cx);
        assert_eq!(multibuffer.read(cx).text(), "XABCD1234\nAB5678");
        multibuffer.group_until_transaction(transaction_1, cx);
        multibuffer.undo(cx);
        assert_eq!(multibuffer.read(cx).text(), "1234\n5678");
        multibuffer.redo(cx);
        assert_eq!(multibuffer.read(cx).text(), "XABCD1234\nAB5678");
    });
}

fn validate_excerpts(
    actual: &[(ExcerptId, BufferId, Range<Anchor>)],
    expected: &Vec<(ExcerptId, BufferId, Range<Anchor>)>,
) {
    assert_eq!(actual.len(), expected.len());

    actual
        .iter()
        .zip(expected)
        .map(|(actual, expected)| {
            assert_eq!(actual.0, expected.0);
            assert_eq!(actual.1, expected.1);
            assert_eq!(actual.2.start, expected.2.start);
            assert_eq!(actual.2.end, expected.2.end);
        })
        .collect_vec();
}

fn map_range_from_excerpt(
    snapshot: &MultiBufferSnapshot,
    excerpt_id: ExcerptId,
    excerpt_buffer: &BufferSnapshot,
    range: Range<usize>,
) -> Range<Anchor> {
    snapshot
        .anchor_in_excerpt(excerpt_id, excerpt_buffer.anchor_before(range.start))
        .unwrap()
        ..snapshot
            .anchor_in_excerpt(excerpt_id, excerpt_buffer.anchor_after(range.end))
            .unwrap()
}

fn make_expected_excerpt_info(
    snapshot: &MultiBufferSnapshot,
    cx: &mut AppContext,
    excerpt_id: ExcerptId,
    buffer: &Model<Buffer>,
    range: Range<usize>,
) -> (ExcerptId, BufferId, Range<Anchor>) {
    (
        excerpt_id,
        buffer.read(cx).remote_id(),
        map_range_from_excerpt(snapshot, excerpt_id, &buffer.read(cx).snapshot(), range),
    )
}

fn format_diff(text: &str, row_infos: &Vec<RowInfo>) -> String {
    let has_diff = row_infos.iter().any(|info| info.diff_status.is_some());
    text.split('\n')
        .zip(row_infos)
        .map(|(line, info)| {
            let marker = match info.diff_status {
                Some(DiffHunkStatus::Added) => "+ ",
                Some(DiffHunkStatus::Removed) => "- ",
                Some(DiffHunkStatus::Modified) => unreachable!(),
                None => {
                    if has_diff && !line.is_empty() {
                        "  "
                    } else {
                        ""
                    }
                }
            };
            format!("{marker}{line}")
        })
        .collect::<Vec<_>>()
        .join("\n")
}

#[track_caller]
fn assert_new_snapshot(
    multibuffer: &Model<MultiBuffer>,
    snapshot: &mut MultiBufferSnapshot,
    subscription: &mut Subscription,
    cx: &mut TestAppContext,
    expected_diff: &str,
) {
    let new_snapshot = multibuffer.read_with(cx, |multibuffer, cx| multibuffer.snapshot(cx));
    let actual_text = new_snapshot.text();
    let line_infos = new_snapshot
        .row_infos(MultiBufferRow(0))
        .collect::<Vec<_>>();
    let actual_diff = format_diff(&actual_text, &line_infos);
    pretty_assertions::assert_eq!(expected_diff, actual_diff);
    check_edits(
        snapshot,
        &new_snapshot,
        &subscription.consume().into_inner(),
    );
    *snapshot = new_snapshot;
}

#[track_caller]
fn check_edits(
    old_snapshot: &MultiBufferSnapshot,
    new_snapshot: &MultiBufferSnapshot,
    edits: &[Edit<usize>],
) {
    let mut text = old_snapshot.text();
    let new_text = new_snapshot.text();
    for edit in edits.iter().rev() {
        if !text.is_char_boundary(edit.old.start)
            || !text.is_char_boundary(edit.old.end)
            || !new_text.is_char_boundary(edit.new.start)
            || !new_text.is_char_boundary(edit.new.end)
        {
            panic!(
                "invalid edits: {:?}\nold text: {:?}\nnew text: {:?}",
                edits, text, new_text
            );
        }

        text.replace_range(
            edit.old.start..edit.old.end,
            &new_text[edit.new.start..edit.new.end],
        );
    }

    pretty_assertions::assert_eq!(text, new_text, "invalid edits: {:?}", edits);
}

#[track_caller]
fn assert_chunks_in_ranges(snapshot: &MultiBufferSnapshot) {
    let full_text = snapshot.text();
    for ix in 0..full_text.len() {
        let mut chunks = snapshot.chunks(0..snapshot.len(), false);
        chunks.seek(ix..snapshot.len());
        let tail = chunks.map(|chunk| chunk.text).collect::<String>();
        assert_eq!(tail, &full_text[ix..], "seek to range: {:?}", ix..);
    }
}

#[track_caller]
fn assert_consistent_line_numbers(snapshot: &MultiBufferSnapshot) {
    let all_line_numbers = snapshot.row_infos(MultiBufferRow(0)).collect::<Vec<_>>();
    for start_row in 1..all_line_numbers.len() {
        let line_numbers = snapshot
            .row_infos(MultiBufferRow(start_row as u32))
            .collect::<Vec<_>>();
        assert_eq!(
            line_numbers,
            all_line_numbers[start_row..],
            "start_row: {start_row}"
        );
    }
}

#[track_caller]
fn assert_position_translation(snapshot: &MultiBufferSnapshot) {
    let text = Rope::from(snapshot.text());

    let mut left_anchors = Vec::new();
    let mut right_anchors = Vec::new();
    let mut offsets = Vec::new();
    let mut points = Vec::new();
    for offset in 0..=text.len() + 1 {
        let clipped_left = snapshot.clip_offset(offset, Bias::Left);
        let clipped_right = snapshot.clip_offset(offset, Bias::Right);
        assert_eq!(
            clipped_left,
            text.clip_offset(offset, Bias::Left),
            "clip_offset({offset:?}, Left)"
        );
        assert_eq!(
            clipped_right,
            text.clip_offset(offset, Bias::Right),
            "clip_offset({offset:?}, Right)"
        );
        assert_eq!(
            snapshot.offset_to_point(clipped_left),
            text.offset_to_point(clipped_left),
            "offset_to_point({clipped_left})"
        );
        assert_eq!(
            snapshot.offset_to_point(clipped_right),
            text.offset_to_point(clipped_right),
            "offset_to_point({clipped_right})"
        );
        let anchor_after = snapshot.anchor_after(clipped_left);
        assert_eq!(
            anchor_after.to_offset(snapshot),
            clipped_left,
            "anchor_after({clipped_left}).to_offset"
        );
        let anchor_before = snapshot.anchor_before(clipped_left);
        assert_eq!(
            anchor_before.to_offset(snapshot),
            clipped_left,
            "anchor_before({clipped_left}).to_offset"
        );
        left_anchors.push(anchor_before);
        right_anchors.push(anchor_after);
        offsets.push(clipped_left);
        points.push(text.offset_to_point(clipped_left));
    }

    for row in 0..text.max_point().row {
        for column in 0..text.line_len(row) + 1 {
            let point = Point { row, column };
            let clipped_left = snapshot.clip_point(point, Bias::Left);
            let clipped_right = snapshot.clip_point(point, Bias::Right);
            assert_eq!(
                clipped_left,
                text.clip_point(point, Bias::Left),
                "clip_point({point:?}, Left)"
            );
            assert_eq!(
                clipped_right,
                text.clip_point(point, Bias::Right),
                "clip_point({point:?}, Right)"
            );
            assert_eq!(
                snapshot.point_to_offset(clipped_left),
                text.point_to_offset(clipped_left),
                "point_to_offset({clipped_left:?})"
            );
            assert_eq!(
                snapshot.point_to_offset(clipped_right),
                text.point_to_offset(clipped_right),
                "point_to_offset({clipped_right:?})"
            );
        }
    }

    for (anchors, bias) in [(&left_anchors, Bias::Left), (&right_anchors, Bias::Right)] {
        for (ix, (offset, anchor)) in offsets.iter().zip(anchors).enumerate() {
            if ix > 0 {
                if offset > &offsets[ix - 1] {
                    let prev_anchor = left_anchors[ix - 1];
                    assert!(
                        anchor.cmp(&prev_anchor, snapshot).is_gt(),
                        "anchor({}, {bias:?}).cmp(&anchor({}, {bias:?}).is_gt()",
                        offsets[ix],
                        offsets[ix - 1],
                    );
                    assert!(
                        prev_anchor.cmp(&anchor, snapshot).is_lt(),
                        "anchor({}, {bias:?}).cmp(&anchor({}, {bias:?}).is_lt()",
                        offsets[ix - 1],
                        offsets[ix],
                    );
                }
            }
        }
    }

    assert_eq!(
        snapshot.summaries_for_anchors::<usize, _>(&left_anchors),
        offsets,
        "left_anchors <-> offsets"
    );
    assert_eq!(
        snapshot.summaries_for_anchors::<Point, _>(&left_anchors),
        points,
        "left_anchors <-> points"
    );
    assert_eq!(
        snapshot.summaries_for_anchors::<usize, _>(&right_anchors),
        offsets,
        "right_anchors <-> offsets"
    );
    assert_eq!(
        snapshot.summaries_for_anchors::<Point, _>(&right_anchors),
        points,
        "right_anchors <-> points"
    );
}
