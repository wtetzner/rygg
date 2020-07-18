package org.bovinegenius.cmplang.ast.module;

import org.pcollections.HashPMap;
import org.pcollections.HashTreePMap;

import java.util.function.UnaryOperator;

/**
 * module type SUBST =
 * sig
 *   type t
 *   val identity: t
 *   val add: Ident.t -> path -> t -> t
 *   val path: path -> t -> path
 * end
 */
public interface Subst<LOC, NAME, IDENT extends Ident<NAME, LOC>> extends UnaryOperator<Path<LOC, NAME, IDENT>> {
    public Subst<LOC, NAME, IDENT> with(IDENT ident, Path<LOC, NAME, IDENT> path);
    public Path<LOC, NAME, IDENT> apply(Path<LOC, NAME, IDENT> path);

    public static <LOC, NAME, IDENT extends Ident<NAME, LOC>> Subst<LOC, NAME, IDENT> identity() {
        return Private.SubstImpl.identity();
    }

    public static class Private {
        private static class SubstImpl<LOC, NAME, IDENT extends Ident<NAME, LOC>> implements Subst<LOC, NAME, IDENT> {
            private final HashPMap<IDENT, Path<LOC, NAME, IDENT>> mapping;

            private SubstImpl(HashPMap<IDENT, Path<LOC, NAME, IDENT>> mapping) {
                this.mapping = mapping;
            }

            public static <LOC, NAME, IDENT extends Ident<NAME, LOC>> Subst<LOC, NAME, IDENT> identity() {
                return new SubstImpl<>(HashTreePMap.empty());
            }

            @Override
            public Subst<LOC, NAME, IDENT> with(IDENT ident, Path<LOC, NAME, IDENT> path) {
                return new SubstImpl<>(mapping.plus(ident, path));
            }

            @Override
            public Path<LOC, NAME, IDENT> apply(Path<LOC, NAME, IDENT> path) {
                Path<LOC, NAME, IDENT> substPath = mapping.get(path.getIdent());
                if (substPath == null) {
                    return path;
                }
                return Path.of(substPath.getIdent(), substPath.getComponents().plusAll(path.getComponents()));
            }

        }
    }
}
