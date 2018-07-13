val rec hist = fn []: real list => ( fn (c,d): real * real => 0 )
  | h::t: real list => ( fn (c,d): real * real => if ( h < c+d andalso h > c-d ) then 1+ (hist t (c,d)) else hist t (c,d));
