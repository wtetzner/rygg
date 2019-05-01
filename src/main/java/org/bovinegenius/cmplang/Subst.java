package org.bovinegenius.cmplang;

import java.util.function.UnaryOperator;

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
public interface Subst<NAME, IDENT extends Ident<NAME>> extends UnaryOperator<Path<NAME, IDENT>> {
    public Subst<NAME, IDENT> with(IDENT ident, Path<NAME, IDENT> path);
    public Path<NAME, IDENT> apply(Path<NAME, IDENT> path);

    public static <NAME, IDENT extends Ident<NAME>> Subst<NAME, IDENT> identity() {
        return Private.SubstImpl.identity();
    }

    public static class Private {
        private static class SubstImpl<NAME, IDENT extends Ident<NAME>> implements Subst<NAME, IDENT> {
            private final HashPMap<IDENT, Path<NAME, IDENT>> mapping;

            private SubstImpl(HashPMap<IDENT, Path<NAME, IDENT>> mapping) {
                this.mapping = mapping;
            }

            public static <NAME, IDENT extends Ident<NAME>> Subst<NAME, IDENT> identity() {
                return new SubstImpl<>(HashTreePMap.empty());
            }

            @Override
            public Subst<NAME, IDENT> with(IDENT ident, Path<NAME, IDENT> path) {
                return new SubstImpl<>(mapping.plus(ident, path));
            }

            @Override
            public Path<NAME, IDENT> apply(Path<NAME, IDENT> path) {
                Path<NAME, IDENT> substPath = mapping.get(path.getIdent());
                if (substPath == null) {
                    return path;
                }
                return Path.of(substPath.getIdent(), substPath.getComponents().plusAll(path.getComponents()));
            }

        }
    }
}
