package org.bovinegenius.cmplang.ast.module.error;

import lombok.Builder;
import lombok.Getter;
import lombok.NonNull;

import java.util.Optional;
import java.util.function.Function;

@Builder
public class KindMismatchInSignature<LOC, KIND> implements TypeError<LOC> {
    @Getter @NonNull private final LOC location;
    @NonNull private final KIND kind;
    @NonNull private final KIND manifestKind;

    @Override
    public String displayError(Function<String, Optional<String>> getFileContents) {
        return this.toString();
    }

    @Override
    public String toString() {
        return String.format("%s: Kind mismatch in manifest type specification; kind: %s, manifest kind: %s", this.location, this.kind, this.manifestKind);
    }
}
