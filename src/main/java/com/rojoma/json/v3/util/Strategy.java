package com.rojoma.json.v3.util;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

public enum Strategy {
    /** Keep names as they are in the source code. */
    Identity,
    /** Split on guessed word-boundaries, underscore-separate, and downcase.. */
    Underscore
}
