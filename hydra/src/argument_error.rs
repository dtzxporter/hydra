/// Occurs when an argument to a function was incorrect or not valid at the given time.
#[derive(Debug)]
pub struct ArgumentError(pub String);

impl<T> From<T> for ArgumentError
where
    T: Into<String>,
{
    fn from(value: T) -> Self {
        Self(value.into())
    }
}
