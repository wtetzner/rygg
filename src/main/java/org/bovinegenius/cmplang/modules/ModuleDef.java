package org.bovinegenius.cmplang.modules;

import lombok.Getter;
import lombok.NonNull;
import lombok.ToString;
import org.bovinegenius.cmplang.ast.module.Ident;
import org.bovinegenius.cmplang.ast.module.Subst;

@ToString
public class ModuleDef<LOC extends Comparable<LOC>, NAME, VAL extends Node<LOC, NAME, VAL>, KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>>
        implements NamedNode<LOC, NAME, ModuleDef<LOC, NAME, VAL, KIND, TYPE>> {
    @Getter private final Ident<LOC, NAME> ident;
    @Getter private final ModuleBody<LOC, NAME, VAL, KIND, TYPE> body;

    private ModuleDef(
            final Ident<LOC, NAME> ident,
            final ModuleBody<LOC, NAME, VAL, KIND, TYPE> body
    ) {
        this.ident = ident;
        this.body = body;
    }

    public static <LOC extends Comparable<LOC>, NAME, VAL extends Node<LOC, NAME, VAL>, KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>> ModuleDef<LOC, NAME, VAL, KIND, TYPE> empty(
            @NonNull final Ident<LOC, NAME> ident,
            @NonNull final ModuleBody<LOC, NAME, VAL, KIND, TYPE> body
    ) {
        return new ModuleDef<>(ident, body);
    }

    @Override
    public ModuleDef<LOC, NAME, VAL, KIND, TYPE> subst(Subst<LOC, NAME> subst) {
        return new ModuleDef<>(this.ident, this.body.subst(subst));
    }

    @Override
    public LOC getLocation() {
        return this.ident.getLocation();
    }

}
