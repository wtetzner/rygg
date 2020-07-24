package org.bovinegenius.cmplang.modules;

import lombok.Getter;
import lombok.NonNull;
import lombok.ToString;
import org.bovinegenius.cmplang.ast.module.Path;
import org.bovinegenius.cmplang.ast.module.Subst;
import org.bovinegenius.cmplang.util.Formatted;

import java.util.Optional;

@ToString
public class ModuleTypeRef<LOC extends Comparable<LOC>, NAME, KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>>
        implements Substable<LOC, NAME, ModuleTypeRef<LOC, NAME, KIND, TYPE>>, Formatted {
    @Getter private final Optional<ModuleType<LOC, NAME, KIND, TYPE>> moduleType;
    @Getter private final Optional<Path<LOC, NAME>> path;

    private ModuleTypeRef(
            @NonNull final Optional<ModuleType<LOC, NAME, KIND, TYPE>> moduleType,
            @NonNull final Optional<Path<LOC, NAME>> path
    ) {
        this.moduleType = moduleType;
        this.path = path;
    }

    public static <LOC extends Comparable<LOC>, NAME, KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>> ModuleTypeRef<LOC, NAME, KIND, TYPE> of(
            @NonNull final ModuleType<LOC, NAME, KIND, TYPE> moduleType
    ) {
        return new ModuleTypeRef<>(Optional.of(moduleType), Optional.empty());
    }

    public static <LOC extends Comparable<LOC>, NAME, KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>> ModuleTypeRef<LOC, NAME, KIND, TYPE> of(
            @NonNull final Path<LOC, NAME> path
    ) {
        return new ModuleTypeRef<>(Optional.empty(), Optional.of(path));
    }

    public boolean isModuleType() {
        return this.moduleType.isPresent();
    }

    public boolean isPath() {
        return this.path.isPresent();
    }

    @Override
    public ModuleTypeRef<LOC, NAME, KIND, TYPE> subst(Subst<LOC, NAME> subst) {
        return new ModuleTypeRef<>(
                this.moduleType.map(subst::apply),
                this.path.map(subst::apply)
        );
    }

    @Override
    public String formatted(boolean inline, int indentAmount, int indentLevel) {
        return Formatted.formatFirst(inline, indentAmount, indentLevel, this.moduleType, this.path);
    }

}
