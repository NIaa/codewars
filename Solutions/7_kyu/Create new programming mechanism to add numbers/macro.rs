macro_rules! add {
    ( $( $x:expr ),* ) => {
        {
            let mut s = 0;
            $( s += $x;)*
            s
        }
    };
}