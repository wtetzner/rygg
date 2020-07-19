package org.bovinegenius.cmplang.modules;

import org.bovinegenius.cmplang.ast.module.Subst;

public interface Substable<LOC, NAME, T> {
    T subst(Subst<LOC, NAME> subst);
}
