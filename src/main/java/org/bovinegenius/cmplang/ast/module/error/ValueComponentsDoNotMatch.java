package org.bovinegenius.cmplang.ast.module.error;

import lombok.Builder;
import lombok.Getter;
import lombok.NonNull;

import java.util.Optional;
import java.util.function.Function;

@Builder
public class ValueComponentsDoNotMatch<LOC, VAL> implements TypeError<LOC> {
    @Getter @NonNull private final LOC location;
    @NonNull private final VAL value1;
    @NonNull private final VAL value2;

    @Override
    public String displayError(Function<String, Optional<String>> getFileContents) {
        return this.toString();
    }

    @Override
    public String toString() {
        return String.format("%s: Value components don't match; value1: %s, value2: %s", this.location,  this.value1, this.value2);
    }
}
