package org.bovinegenius.cmplang.ast.module.error;

import lombok.Builder;
import lombok.Getter;
import lombok.NonNull;

import java.util.Optional;
import java.util.function.Function;

@Builder
public class KindMismatchInTypeDefinition<LOC, KIND> implements TypeError<LOC> {
    @Getter @NonNull private final LOC location;
    @NonNull private final KIND kind;
    @NonNull private final KIND definitionKind;

    @Override
    public String displayError(Function<String, Optional<String>> getFileContents) {
        return this.toString();
    }

    @Override
    public String toString() {
        return String.format("%s: Kind mismatch in type definition; kind: %s, definition kind: %s", this.location, this.kind, this.definitionKind);
    }
}
