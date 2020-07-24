package org.bovinegenius.cmplang.modules;

import lombok.Getter;
import lombok.NonNull;
import lombok.ToString;
import org.bovinegenius.cmplang.ast.module.Path;
import org.bovinegenius.cmplang.ast.module.Subst;
import org.bovinegenius.cmplang.util.Formatted;

import java.util.Optional;

@ToString
public class FunctorOutputType<LOC extends Comparable<LOC>, NAME,KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>>
        implements Substable<LOC, NAME, FunctorOutputType<LOC, NAME, KIND, TYPE>>, Formatted {

    @Getter private final Optional<ModuleType<LOC, NAME, KIND, TYPE>> moduleType;
    @Getter private final Optional<FunctorType<LOC, NAME, KIND, TYPE>> functorType;

    private FunctorOutputType(
            @NonNull final Optional<ModuleType<LOC, NAME, KIND, TYPE>> moduleType,
            @NonNull final Optional<FunctorType<LOC, NAME, KIND, TYPE>> functorType
    ) {
        this.moduleType = moduleType;
        this.functorType = functorType;
    }

    public static <LOC extends Comparable<LOC>, NAME,KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>> FunctorOutputType<LOC, NAME, KIND, TYPE> of(
            @NonNull final ModuleType<LOC, NAME, KIND, TYPE> moduleType
    ) {
        return new FunctorOutputType<>(Optional.of(moduleType), Optional.empty());
    }

    public static <LOC extends Comparable<LOC>, NAME,KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>> FunctorOutputType<LOC, NAME, KIND, TYPE> of(
            @NonNull final FunctorType<LOC, NAME, KIND, TYPE> functorType
    ) {
        return new FunctorOutputType<>(Optional.empty(), Optional.of(functorType));
    }

    @Override
    public FunctorOutputType<LOC, NAME, KIND, TYPE> subst(Subst<LOC, NAME> subst) {
        return new FunctorOutputType<>(this.moduleType.map(subst::apply), this.functorType.map(subst::apply));
    }

    @Override
    public String formatted(boolean inline, int indentAmount, int indentLevel) {
        return Formatted.formatFirst(inline, indentAmount, indentLevel, this.moduleType, this.functorType);
    }

}
