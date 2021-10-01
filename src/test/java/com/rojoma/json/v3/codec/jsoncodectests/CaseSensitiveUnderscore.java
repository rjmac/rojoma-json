package com.rojoma.json.v3.codec.jsoncodectests;

import com.rojoma.json.v3.util.JsonEnumStrategy;
import com.rojoma.json.v3.util.Strategy;

@JsonEnumStrategy(Strategy.Underscore)
public enum CaseSensitiveUnderscore {
    A, B, HelloWorld
}
