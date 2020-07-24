package org.bovinegenius.cmplang.modules;

import lombok.Getter;
import lombok.NonNull;
import lombok.ToString;
import org.bovinegenius.cmplang.ast.module.Ident;
import org.bovinegenius.cmplang.ast.module.Subst;

@ToString
public class ModuleTypeDef<LOC extends Comparable<LOC>, NAME, KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>>
        implements NamedNode<LOC, NAME, ModuleTypeDef<LOC, NAME, KIND, TYPE>> {
    @Getter private final Ident<LOC, NAME> ident;
    private final ModuleType<LOC, NAME, KIND, TYPE> body;

    private ModuleTypeDef(
            final Ident<LOC, NAME> ident,
            final ModuleType<LOC, NAME, KIND, TYPE> body
    ) {
        this.ident = ident;
        this.body = body;
    }

    public static <LOC extends Comparable<LOC>, NAME, KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>> ModuleTypeDef<LOC, NAME, KIND, TYPE> empty(
            @NonNull Ident<LOC, NAME> ident,
            @NonNull final ModuleType<LOC, NAME, KIND, TYPE> body
    ) {
        return new ModuleTypeDef<>(ident, body);
    }

    @Override
    public ModuleTypeDef<LOC, NAME, KIND, TYPE> subst(Subst<LOC, NAME> subst) {
        return new ModuleTypeDef<>(this.ident, body.subst(subst));
    }

    @Override
    public LOC getLocation() {
        return this.ident.getLocation();
    }
}
