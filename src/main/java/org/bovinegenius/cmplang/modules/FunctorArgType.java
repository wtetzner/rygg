package org.bovinegenius.cmplang.modules;

import lombok.Getter;
import lombok.NonNull;
import lombok.ToString;
import org.bovinegenius.cmplang.ast.module.Path;
import org.bovinegenius.cmplang.ast.module.Subst;
import org.bovinegenius.cmplang.util.Formatted;

import java.util.Optional;

@ToString
public class FunctorArgType<LOC extends Comparable<LOC>, NAME,KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>>
        implements Substable<LOC, NAME, FunctorArgType<LOC, NAME, KIND, TYPE>>, Formatted {

    @Getter private final Optional<ModuleType<LOC, NAME, KIND, TYPE>> moduleType;
    @Getter private final Optional<FunctorType<LOC, NAME, KIND, TYPE>> functorType;
    @Getter private final Optional<Path<LOC, NAME>> path;

    private FunctorArgType(
            @NonNull final Optional<ModuleType<LOC, NAME, KIND, TYPE>> moduleType,
            @NonNull final Optional<FunctorType<LOC, NAME, KIND, TYPE>> functorType,
            @NonNull final Optional<Path<LOC, NAME>> path
    ) {
        this.moduleType = moduleType;
        this.functorType = functorType;
        this.path = path;
    }

    public static <LOC extends Comparable<LOC>, NAME,KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>> FunctorArgType<LOC, NAME, KIND, TYPE> of(
            @NonNull final ModuleType<LOC, NAME, KIND, TYPE> moduleType
    ) {
        return new FunctorArgType<>(Optional.of(moduleType), Optional.empty(), Optional.empty());
    }

    public static <LOC extends Comparable<LOC>, NAME,KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>> FunctorArgType<LOC, NAME, KIND, TYPE> of(
            @NonNull final FunctorType<LOC, NAME, KIND, TYPE> functorType
    ) {
        return new FunctorArgType<>(Optional.empty(), Optional.of(functorType), Optional.empty());
    }

    public static <LOC extends Comparable<LOC>, NAME,KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>> FunctorArgType<LOC, NAME, KIND, TYPE> of(
            @NonNull final Path<LOC, NAME> path
    ) {
        return new FunctorArgType<>(Optional.empty(), Optional.empty(), Optional.of(path));
    }

    @Override
    public FunctorArgType<LOC, NAME, KIND, TYPE> subst(Subst<LOC, NAME> subst) {
        return new FunctorArgType<>(this.moduleType.map(subst::apply), this.functorType.map(subst::apply), this.path.map(subst::apply));
    }

    @Override
    public String formatted(boolean inline, int indentAmount, int indentLevel) {
        return Formatted.formatFirst(inline, indentAmount, indentLevel, this.moduleType, this.functorType, this.path);
    }

}
