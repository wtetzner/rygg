package org.bovinegenius.cmplang.ast.module.error;

import lombok.Builder;
import lombok.Getter;
import lombok.NonNull;
import org.bovinegenius.cmplang.ast.module.Ident;
import org.bovinegenius.cmplang.ast.module.ModuleType;
import org.bovinegenius.cmplang.ast.module.Specification;

import java.util.Optional;
import java.util.function.Function;

@Builder
public class UnmatchedSignatureComponent<LOC, NAME, IDENT extends Ident<NAME, LOC>, VAL, KIND, DEF, TERM> implements TypeError<LOC> {
    @Getter @NonNull private final LOC location;
    @NonNull private final Specification<LOC, NAME, IDENT, VAL, KIND, DEF> spec;
    @NonNull private final ModuleType.Signature<LOC, NAME, IDENT, VAL, KIND, DEF> sig1;
    @NonNull private final ModuleType.Signature<LOC, NAME, IDENT, VAL, KIND, DEF> sig2;

    @Override
    public String displayError(Function<String, Optional<String>> getFileContents) {
        return this.toString();
    }

    @Override
    public String toString() {
        return String.format("%s: Unmatched signature component: %s; sig1: %s, sig2: %s", this.location, this.spec, this.sig1, this.sig2);
    }
}
