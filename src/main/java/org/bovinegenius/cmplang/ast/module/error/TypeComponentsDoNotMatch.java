package org.bovinegenius.cmplang.ast.module.error;

import lombok.Builder;
import lombok.Getter;
import lombok.NonNull;
import org.bovinegenius.cmplang.ast.module.Ident;
import org.bovinegenius.cmplang.ast.module.Specification;

import java.util.Optional;
import java.util.function.Function;

@Builder
public class TypeComponentsDoNotMatch<LOC, NAME, IDENT extends Ident<NAME, LOC>, VAL, KIND, DEF> implements TypeError<LOC> {
    @Getter @NonNull private final LOC location;
    @NonNull private final Specification.TypeSig<LOC, NAME, IDENT, VAL, KIND, DEF> value1;
    @NonNull private final Specification.TypeSig<LOC, NAME, IDENT, VAL, KIND, DEF> value2;

    @Override
    public String displayError(Function<String, Optional<String>> getFileContents) {
        return this.toString();
    }

    @Override
    public String toString() {
        return String.format("%s: Type components don't match; value1: %s, value2: %s", this.location,  this.value1, this.value2);
    }
}
