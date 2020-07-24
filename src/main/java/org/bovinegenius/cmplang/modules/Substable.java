package org.bovinegenius.cmplang.modules;

import org.bovinegenius.cmplang.ast.module.Subst;

public interface Substable<LOC extends Comparable<LOC>, NAME, SELF> {
    SELF subst(Subst<LOC, NAME> subst);
}
