package org.bovinegenius.cmplang.modules;

import lombok.Getter;
import lombok.NonNull;
import lombok.ToString;
import org.bovinegenius.cmplang.ast.module.Ident;
import org.bovinegenius.cmplang.ast.module.Subst;

@ToString
public class FunctorDef<LOC extends Comparable<LOC>, NAME, VAL extends Node<LOC, NAME, VAL>, KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>>
        implements NamedNode<LOC, NAME, FunctorDef<LOC, NAME, VAL, KIND, TYPE>> {

    @Getter private final Ident<LOC, NAME> ident;
    private final Functor<LOC, NAME, VAL, KIND, TYPE> functor;

    private FunctorDef(
            @NonNull final Ident<LOC, NAME> ident,
            @NonNull final Functor<LOC, NAME, VAL, KIND, TYPE> functor
    ) {
        this.ident = ident;
        this.functor = functor;
    }

    public static <LOC extends Comparable<LOC>, NAME, VAL extends Node<LOC, NAME, VAL>, KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>, ARGT extends FunctorArgType<LOC, NAME, KIND, TYPE>> FunctorDef<LOC, NAME, VAL, KIND, TYPE> of(
            @NonNull final Ident<LOC, NAME> ident,
            @NonNull final Functor<LOC, NAME, VAL, KIND, TYPE> functor
    ) {
        return new FunctorDef<>(ident, functor);
    }

    @Override
    public FunctorDef<LOC, NAME, VAL, KIND, TYPE> subst(Subst<LOC, NAME> subst) {
        return new FunctorDef<>(
                this.ident,
                this.functor.subst(subst)
        );
    }

    @Override
    public LOC getLocation() {
        return this.ident.getLocation();
    }

    @Override
    public String formatted(boolean inline, int indentAmount, int indentLevel) {
        StringBuilder sb = new StringBuilder();
        sb.append("module ").append(ident.formatted(true, indentAmount, indentLevel));
        sb.append(this.functor.formatted(inline, indentAmount, indentLevel));
        return sb.toString();
    }

}
