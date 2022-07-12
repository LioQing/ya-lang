use super::*;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct RepeatsKind<T> {
    pub value: Result<T, ErrorKind>,
    pub next: Option<Box<Repeats<T>>>,
}

impl<T> RepeatsKind<T> {
    pub fn push(&mut self, repeat: SynResult<Repeat<T>>) {
        let mut curr = self;
        while curr.next.is_some() {
            curr = &mut curr.next.as_mut().unwrap().as_mut().value;
        }

        let (value, span) = match repeat {
            Ok(repeat) => (Ok(repeat.value.value), repeat.span),
            Err(err) => (Err(err.value), err.span),
        };

        curr.next = Some(Box::new(Repeats {
            value: RepeatsKind {
                value,
                next: None,
            },
            span,
        }));
    }
}

pub type Repeats<T> = Spanned<RepeatsKind<T>>;

pub trait RepeatsIntoValueVecExt<T> {
    fn into_value_vec(self) -> Vec<SynResult<Spanned<T>>>;
}

impl<T> RepeatsIntoValueVecExt<T> for Repeats<T> {
    fn into_value_vec(self) -> Vec<SynResult<Spanned<T>>> {
        self
            .into_iter()
            .map(|repeat| repeat
                .map(|repeat| Spanned::<T>::new(repeat.value.value, repeat.span))
            )
            .collect()
    }
}

pub struct RepeatsIntoIter<T> {
    curr: Option<Repeats<T>>,
}

pub trait RepeatsIntoIterExt<T> {
    fn into_iter(self) -> RepeatsIntoIter<T>;
}

impl<T> RepeatsIntoIterExt<T> for Repeats<T> {
    fn into_iter(self) -> RepeatsIntoIter<T> {
        RepeatsIntoIter {
            curr: Some(self),
        }
    }
}

impl<T> Iterator for RepeatsIntoIter<T> {
    type Item = SynResult<Repeat<T>>;

    fn next(&mut self) -> Option<Self::Item> {
        let curr = self.curr.take()?;
        let res = match curr.value.value {
            Ok(expr) => Ok(Repeat::new(expr.into(), curr.span)),
            Err(err) => Err(Error::new(err, curr.span)),
        };

        self.curr = curr.value.next.map(|next| *next);
        
        Some(res)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct RepeatKind<T> {
    pub value: T,
}

impl<T> From<T> for RepeatKind<T> {
    fn from(value: T) -> Self {
        Self { value }
    }
}

pub type Repeat<T> = Spanned<RepeatKind<T>>;
