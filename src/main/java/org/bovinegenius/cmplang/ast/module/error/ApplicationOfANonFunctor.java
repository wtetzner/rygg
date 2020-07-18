package org.bovinegenius.cmplang.ast.module.error;

import lombok.Builder;
import lombok.Getter;
import lombok.NonNull;
import org.bovinegenius.cmplang.ast.module.Ident;
import org.bovinegenius.cmplang.ast.module.ModuleTerm;

import java.util.Optional;
import java.util.function.Function;

@Builder
public class ApplicationOfANonFunctor<LOC, NAME, IDENT extends Ident<NAME, LOC>, VAL, KIND, DEF, TERM> implements TypeError<LOC> {
    @Getter @NonNull private final LOC location;
    @NonNull private final ModuleTerm.Apply<LOC, NAME, IDENT, VAL, KIND, DEF, TERM> apply;

    @Override
    public String displayError(Function<String, Optional<String>> getFileContents) {
        return this.toString();
    }

    @Override
    public String toString() {
        return String.format("%s: Application of a non-functor: %s", this.location, this.apply);
    }
}
