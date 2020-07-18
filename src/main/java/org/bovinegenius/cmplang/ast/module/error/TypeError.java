package org.bovinegenius.cmplang.ast.module.error;

import org.bovinegenius.cmplang.util.Result;

import java.util.Optional;
import java.util.function.Function;

public interface TypeError<LOC> {
    LOC getLocation();
    String displayError(Function<String, Optional<String>> getFileContents);

    default <V> Result<V, TypeError<LOC>>  err() {
        return Result.err(this);
    }
}
