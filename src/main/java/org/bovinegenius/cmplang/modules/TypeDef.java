package org.bovinegenius.cmplang.modules;

import lombok.NonNull;
import lombok.Value;
import org.bovinegenius.cmplang.ast.module.Subst;
import org.bovinegenius.cmplang.util.Formatted;

import java.util.Optional;

@Value(staticConstructor = "of")
public class TypeDef<LOC extends Comparable<LOC>, NAME, KIND extends Node<LOC, NAME, KIND>, TYPE extends Node<LOC, NAME, TYPE>>
        implements Substable<LOC, NAME, TypeDef<LOC, NAME, KIND, TYPE>> {
    @NonNull KIND kind;
    @NonNull Optional<TYPE> concreteType;

    @Override
    public TypeDef<LOC, NAME, KIND, TYPE> subst(Subst<LOC, NAME> subst) {
        return TypeDef.of(kind.subst(subst), concreteType.map(t -> t.subst(subst)));
    }

}
