package com.rojoma.json.v3.codec.jsoncodectests;

import com.rojoma.json.v3.util.JsonCaseInsensitiveEnum;
import com.rojoma.json.v3.util.JsonEnumStrategy;
import com.rojoma.json.v3.util.Strategy;

@JsonCaseInsensitiveEnum
@JsonEnumStrategy(Strategy.Underscore)
public enum CaseInsensitiveUnderscore {
    A, B, HelloWorld
}
