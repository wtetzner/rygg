package org.bovinegenius.cmplang.ast.module.error;

import lombok.Builder;
import lombok.Getter;
import lombok.NonNull;
import org.bovinegenius.cmplang.ast.module.Ident;

import java.util.Optional;
import java.util.function.Function;

@Builder
public class RepeatedValueName<LOC, NAME, IDENT extends Ident<NAME, LOC>> implements TypeError<LOC> {
    @Getter @NonNull private final LOC location;
    @NonNull private final IDENT original;
    @NonNull private final IDENT duplicate;

    @Override
    public String displayError(Function<String, Optional<String>> getFileContents) {
        return this.toString();
    }

    @Override
    public String toString() {
        return String.format("Found duplicate value name; original: %s, duplicate: %s", this.original, this.duplicate);
    }
}
