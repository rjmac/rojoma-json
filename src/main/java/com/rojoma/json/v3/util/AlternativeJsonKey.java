package com.rojoma.json.v3.util;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.annotation.Repeatable;

@Repeatable(AlternativeJsonKeys.class)
@Retention(RetentionPolicy.CLASS)
@Target({ElementType.PARAMETER})
public @interface AlternativeJsonKey {
    String value();
}
