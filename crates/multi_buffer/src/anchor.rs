use super::{ExcerptId, MultiBufferSnapshot, ToOffset, ToOffsetUtf16, ToPoint};
use language::{OffsetUtf16, Point, TextDimension};
use std::{
    cmp::Ordering,
    ops::{Range, Sub},
};
use sum_tree::Bias;
use text::BufferId;

#[derive(Clone, Copy, Eq, PartialEq, Debug, Hash)]
pub struct Anchor {
    pub buffer_id: Option<BufferId>,
    pub excerpt_id: ExcerptId,
    pub text_anchor: text::Anchor,
    pub diff_base_anchor: Option<DiffBaseAnchor>,
}

#[derive(Clone, Copy, Eq, PartialEq, Debug, Hash)]
pub struct DiffBaseAnchor {
    pub text_anchor: text::Anchor,
    pub version: usize,
}

impl Anchor {
    pub fn in_buffer(
        excerpt_id: ExcerptId,
        buffer_id: BufferId,
        text_anchor: text::Anchor,
    ) -> Self {
        Self {
            buffer_id: Some(buffer_id),
            excerpt_id,
            text_anchor,
            diff_base_anchor: None,
        }
    }

    pub fn min() -> Self {
        Self {
            buffer_id: None,
            excerpt_id: ExcerptId::min(),
            text_anchor: text::Anchor::MIN,
            diff_base_anchor: None,
        }
    }

    pub fn max() -> Self {
        Self {
            buffer_id: None,
            excerpt_id: ExcerptId::max(),
            text_anchor: text::Anchor::MAX,
            diff_base_anchor: None,
        }
    }

    pub fn cmp(&self, other: &Anchor, snapshot: &MultiBufferSnapshot) -> Ordering {
        let excerpt_id_cmp = self.excerpt_id.cmp(&other.excerpt_id, snapshot);
        if excerpt_id_cmp.is_ne() {
            return excerpt_id_cmp;
        }
        if self.excerpt_id == ExcerptId::min() || self.excerpt_id == ExcerptId::max() {
            return Ordering::Equal;
        }
        if let Some(excerpt) = snapshot.excerpt(self.excerpt_id) {
            let text_cmp = self.text_anchor.cmp(&other.text_anchor, &excerpt.buffer);
            if text_cmp.is_ne() {
                return text_cmp;
            }
            if self.diff_base_anchor.is_some() || other.diff_base_anchor.is_some() {
                if let Some(diff_base) = snapshot.diffs.get(&excerpt.buffer_id) {
                    let self_anchor = self.diff_base_anchor.and_then(|a| {
                        (a.version == diff_base.base_text_version).then(|| a.text_anchor)
                    });
                    let other_anchor = other.diff_base_anchor.and_then(|a| {
                        (a.version == diff_base.base_text_version).then(|| a.text_anchor)
                    });
                    return match (self_anchor, other_anchor) {
                        (Some(a), Some(b)) => a.cmp(&b, &diff_base.base_text),
                        (Some(_), None) => match other.text_anchor.bias {
                            Bias::Left => Ordering::Greater,
                            Bias::Right => Ordering::Less,
                        },
                        (None, Some(_)) => match self.text_anchor.bias {
                            Bias::Left => Ordering::Less,
                            Bias::Right => Ordering::Greater,
                        },
                        (None, None) => Ordering::Equal,
                    };
                }
            }
        }
        Ordering::Equal
    }

    pub fn bias(&self) -> Bias {
        self.text_anchor.bias
    }

    pub fn bias_left(&self, snapshot: &MultiBufferSnapshot) -> Anchor {
        if self.text_anchor.bias != Bias::Left {
            if let Some(excerpt) = snapshot.excerpt(self.excerpt_id) {
                return Self {
                    buffer_id: self.buffer_id,
                    excerpt_id: self.excerpt_id,
                    text_anchor: self.text_anchor.bias_left(&excerpt.buffer),
                    diff_base_anchor: self.diff_base_anchor.map(|mut a| {
                        if let Some(base) = snapshot.diffs.get(&excerpt.buffer_id) {
                            if a.version == base.base_text_version {
                                a.text_anchor = a.text_anchor.bias_left(&base.base_text)
                            }
                        }
                        a
                    }),
                };
            }
        }
        *self
    }

    pub fn bias_right(&self, snapshot: &MultiBufferSnapshot) -> Anchor {
        if self.text_anchor.bias != Bias::Right {
            if let Some(excerpt) = snapshot.excerpt(self.excerpt_id) {
                return Self {
                    buffer_id: self.buffer_id,
                    excerpt_id: self.excerpt_id,
                    text_anchor: self.text_anchor.bias_right(&excerpt.buffer),
                    diff_base_anchor: self.diff_base_anchor.map(|mut a| {
                        if let Some(base) = snapshot.diffs.get(&excerpt.buffer_id) {
                            if a.version == base.base_text_version {
                                a.text_anchor = a.text_anchor.bias_right(&base.base_text)
                            }
                        }
                        a
                    }),
                };
            }
        }
        *self
    }

    pub fn summary<D>(&self, snapshot: &MultiBufferSnapshot) -> D
    where
        D: TextDimension + Ord + Sub<D, Output = D> + std::fmt::Debug,
    {
        snapshot.summary_for_anchor(self)
    }

    pub fn is_valid(&self, snapshot: &MultiBufferSnapshot) -> bool {
        if *self == Anchor::min() || *self == Anchor::max() {
            true
        } else if let Some(excerpt) = snapshot.excerpt(self.excerpt_id) {
            excerpt.contains(self)
                && (self.text_anchor == excerpt.range.context.start
                    || self.text_anchor == excerpt.range.context.end
                    || self.text_anchor.is_valid(&excerpt.buffer))
        } else {
            false
        }
    }
}

impl ToOffset for Anchor {
    fn to_offset(&self, snapshot: &MultiBufferSnapshot) -> usize {
        self.summary(snapshot)
    }
}

impl ToOffsetUtf16 for Anchor {
    fn to_offset_utf16(&self, snapshot: &MultiBufferSnapshot) -> OffsetUtf16 {
        self.summary(snapshot)
    }
}

impl ToPoint for Anchor {
    fn to_point<'a>(&self, snapshot: &MultiBufferSnapshot) -> Point {
        self.summary(snapshot)
    }
}

pub trait AnchorRangeExt {
    fn cmp(&self, b: &Range<Anchor>, buffer: &MultiBufferSnapshot) -> Ordering;
    fn overlaps(&self, b: &Range<Anchor>, buffer: &MultiBufferSnapshot) -> bool;
    fn to_offset(&self, content: &MultiBufferSnapshot) -> Range<usize>;
    fn to_point(&self, content: &MultiBufferSnapshot) -> Range<Point>;
}

impl AnchorRangeExt for Range<Anchor> {
    fn cmp(&self, other: &Range<Anchor>, buffer: &MultiBufferSnapshot) -> Ordering {
        match self.start.cmp(&other.start, buffer) {
            Ordering::Equal => other.end.cmp(&self.end, buffer),
            ord => ord,
        }
    }

    fn overlaps(&self, other: &Range<Anchor>, buffer: &MultiBufferSnapshot) -> bool {
        self.end.cmp(&other.start, buffer).is_ge() && self.start.cmp(&other.end, buffer).is_le()
    }

    fn to_offset(&self, content: &MultiBufferSnapshot) -> Range<usize> {
        self.start.to_offset(content)..self.end.to_offset(content)
    }

    fn to_point(&self, content: &MultiBufferSnapshot) -> Range<Point> {
        self.start.to_point(content)..self.end.to_point(content)
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug, Hash, Ord, PartialOrd)]
pub struct Offset(pub usize);
