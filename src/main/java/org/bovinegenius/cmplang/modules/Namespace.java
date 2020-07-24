package org.bovinegenius.cmplang.modules;

import lombok.ToString;
import org.bovinegenius.cmplang.ast.module.Subst;

@ToString
public class Namespace<LOC extends Comparable<LOC>, NAME, VAL extends Node<LOC, NAME, VAL>, KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>>
        implements Substable<LOC, NAME, Namespace<LOC, NAME, VAL, KIND, TYPE>> {
    private final IdentMap<LOC, NAME, NamespaceDeclaration<LOC, NAME, VAL, KIND, TYPE>> namespaces;

    private Namespace(
            final IdentMap<LOC, NAME, NamespaceDeclaration<LOC, NAME, VAL, KIND, TYPE>> namespaces
    ) {
        this.namespaces = namespaces;
    }

    public Namespace<LOC, NAME, VAL, KIND, TYPE> withNamespaceDeclaration(
            NamespaceDeclaration<LOC, NAME, VAL, KIND, TYPE> decl
    ) {
        return new Namespace<>(this.namespaces.plus(decl.getIdent(), decl));
    }

    @Override
    public Namespace<LOC, NAME, VAL, KIND, TYPE> subst(Subst<LOC, NAME> subst) {
        return new Namespace<>(this.namespaces.subst(subst));
    }
}
