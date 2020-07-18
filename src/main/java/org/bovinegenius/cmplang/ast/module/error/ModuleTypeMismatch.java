package org.bovinegenius.cmplang.ast.module.error;

import lombok.Builder;
import lombok.Getter;
import lombok.NonNull;
import org.bovinegenius.cmplang.ast.module.Ident;
import org.bovinegenius.cmplang.ast.module.ModuleType;

import java.util.Optional;
import java.util.function.Function;

@Builder
public class ModuleTypeMismatch<LOC, NAME, IDENT extends Ident<NAME, LOC>, VAL, KIND, DEF, TERM> implements TypeError<LOC> {
    @Getter @NonNull private final LOC location;
    @NonNull ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> modType1;
    @NonNull ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> modType2;

    @Override
    public String displayError(Function<String, Optional<String>> getFileContents) {
        return this.toString();
    }

    @Override
    public String toString() {
        return String.format("%s: Module type mismatch: %s vs %s", this.location, this.modType1, this.modType2);
    }
}
