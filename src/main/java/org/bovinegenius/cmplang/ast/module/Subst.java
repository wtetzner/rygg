package org.bovinegenius.cmplang.ast.module;

import org.bovinegenius.cmplang.modules.Substable;
import org.pcollections.HashPMap;
import org.pcollections.HashTreePMap;

/**
 * module type SUBST =
 * sig
 *   type t
 *   val identity: t
 *   val add: Ident.t -> path -> t -> t
 *   val path: path -> t -> path
 * end
 */
public class Subst<LOC extends Comparable<LOC>, NAME> {
    private final HashPMap<Ident<LOC, NAME>, Path<LOC, NAME>> mapping;

    private Subst(HashPMap<Ident<LOC, NAME>, Path<LOC, NAME>> mapping) {
        this.mapping = mapping;
    }

    public static <LOC extends Comparable<LOC>, NAME> Subst<LOC, NAME> identity() {
        return new Subst<>(HashTreePMap.empty());
    }

    public Subst<LOC, NAME> with(Ident<LOC, NAME> ident, Path<LOC, NAME> path) {
        return new Subst<>(mapping.plus(ident, path));
    }

    public Path<LOC, NAME> apply(Path<LOC, NAME> path) {
        Path<LOC, NAME> substPath = mapping.get(path.getIdent());
        if (substPath == null) {
            return path;
        }
        return Path.of(substPath.getIdent(), substPath.getComponents().plusAll(path.getComponents()));
    }

    public <T extends Substable<LOC, NAME, T>> T apply(T obj) {
        if (null == obj) {
            return null;
        }
        return obj.subst(this);
    }

}

