package org.bovinegenius.cmplang.ast.module.error;

import lombok.Builder;
import lombok.Getter;
import lombok.NonNull;
import org.bovinegenius.cmplang.ast.module.Ident;

import java.util.Optional;
import java.util.function.Function;

@Builder
public class UndelcaredType<LOC, DEF> implements TypeError<LOC> {
    @Getter @NonNull private final LOC location;
    @NonNull private final DEF type;

    @Override
    public String displayError(Function<String, Optional<String>> getFileContents) {
        return this.toString();
    }

    @Override
    public String toString() {
        return String.format("%s: Undeclared type: %s", this.location, this.type);
    }
}
