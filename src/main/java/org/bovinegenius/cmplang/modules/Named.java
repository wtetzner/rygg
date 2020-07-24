package org.bovinegenius.cmplang.modules;

import org.bovinegenius.cmplang.ast.module.Ident;

public interface Named<LOC extends Comparable<LOC>, NAME> {
    Ident<LOC, NAME> getIdent();
}
