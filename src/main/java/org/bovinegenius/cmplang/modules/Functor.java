package org.bovinegenius.cmplang.modules;

import lombok.NonNull;
import lombok.ToString;
import org.bovinegenius.cmplang.ast.module.Ident;
import org.bovinegenius.cmplang.ast.module.Subst;
import org.bovinegenius.cmplang.util.Formatted;

import java.util.Optional;

@ToString
public class Functor<LOC extends Comparable<LOC>, NAME, VAL extends Node<LOC, NAME, VAL>, KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>>
        implements Substable<LOC, NAME, Functor<LOC, NAME, VAL, KIND, TYPE>>, Formatted {

    private final Ident<LOC, NAME> argName;
    private final FunctorArgType<LOC, NAME, KIND, TYPE> argType;
    private final Optional<ModuleBody<LOC, NAME, VAL, KIND, TYPE>> moduleBody;
    private final Optional<Functor<LOC, NAME, VAL, KIND, TYPE>> functorBody;

    private Functor(
            @NonNull final Ident<LOC, NAME> argName,
            @NonNull final FunctorArgType<LOC, NAME, KIND, TYPE> argType,
            @NonNull final Optional<ModuleBody<LOC, NAME, VAL, KIND, TYPE>> moduleBody,
            @NonNull final Optional<Functor<LOC, NAME, VAL, KIND, TYPE>> functorBody
    ) {
        this.argName = argName;
        this.argType = argType;
        this.moduleBody = moduleBody;
        this.functorBody = functorBody;
    }

    public static <LOC extends Comparable<LOC>, NAME, VAL extends Node<LOC, NAME, VAL>, KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>> Functor<LOC, NAME, VAL, KIND, TYPE> of(
            @NonNull final Ident<LOC, NAME> argName,
            @NonNull final FunctorArgType<LOC, NAME, KIND, TYPE> argType,
            @NonNull final ModuleBody<LOC, NAME, VAL, KIND, TYPE> moduleBody
    ) {
        return new Functor<>(argName, argType, Optional.of(moduleBody), Optional.empty());
    }

    public static <LOC extends Comparable<LOC>, NAME, VAL extends Node<LOC, NAME, VAL>, KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>> Functor<LOC, NAME, VAL, KIND, TYPE> of(
            @NonNull final Ident<LOC, NAME> argName,
            @NonNull final FunctorArgType<LOC, NAME, KIND, TYPE> argType,
            @NonNull final Functor<LOC, NAME, VAL, KIND, TYPE> functorBody
    ) {
        return new Functor<>(argName, argType, Optional.empty(), Optional.of(functorBody));
    }

    @Override
    public Functor<LOC, NAME, VAL, KIND, TYPE> subst(Subst<LOC, NAME> subst) {
        return new Functor<>(
                this.argName,
                this.argType.subst(subst),
                this.moduleBody.map(subst::apply),
                this.functorBody.map(subst::apply)
        );
    }

    @Override
    public String formatted(boolean inline, int indentAmount, int indentLevel) {
        StringBuilder sb = new StringBuilder();
        sb.append("(").append(argName.formatted(true, indentAmount, indentLevel)).append(": ")
                .append(this.argType.formatted(true, indentAmount, indentLevel))
                .append(")");
        if (this.moduleBody.isPresent()) {
            sb.append(this.moduleBody.get().formatted(inline, indentAmount, indentLevel));
        } else {
            sb.append(this.functorBody.get().formatted(inline, indentAmount, indentLevel));
        }

        return sb.toString();
    }
}
