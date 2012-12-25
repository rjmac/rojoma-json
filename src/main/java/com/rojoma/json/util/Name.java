package com.rojoma.json.util;

import java.lang.annotation.*;

@Retention(RetentionPolicy.SOURCE)
@Target({ElementType.METHOD})
public @interface Name {
    String value();
}
