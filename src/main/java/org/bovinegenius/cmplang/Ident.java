package org.bovinegenius.cmplang;

import java.util.concurrent.atomic.AtomicLong;

/**
 * module type IDENT =
 * sig
 *   type t
 *   val create: string -> t
 *   val name: t -> string
 *   val equal: t -> t -> bool
 *   type 'a tbl
 *   val emptytbl: 'a tbl
 *   val add: t -> 'a -> 'a tbl -> 'a tbl
 *   val find: t -> 'a tbl -> 'a
 * end
 */
public interface Ident<NAME> extends HasLocation {
    public NAME getName();

    public static <NAME> Ident<NAME> create(NAME name) {
        return Private.IdentImpl.create(name);
    }

    public static class Private {
        private static class IdentImpl<NAME> implements Ident<NAME> {
            private static final AtomicLong STAMP_SEQ = new AtomicLong(1);

            private final NAME name;
            private final long stamp;

            private IdentImpl(NAME name, long stamp) {
                this.name = name;
                this.stamp = stamp;
            }

            public NAME getName() {
                return this.name;
            }

            public static <NAME> Ident<NAME> create(NAME name) {
                return new IdentImpl<>(name, STAMP_SEQ.getAndIncrement());
            }

            @Override
            public int hashCode() {
                return Long.hashCode(this.stamp);
            }

            @Override
            public boolean equals(Object other) {
                if (other == null) {
                    return false;
                }
                if (other instanceof IdentImpl) {
                    return this.stamp == ((IdentImpl<NAME>) other).stamp;
                }
                throw new IllegalArgumentException(String.format("Cannot compare object of type %s against object of type %s: %s vs %s",
                        this.getClass().getCanonicalName(),
                        other.getClass().getCanonicalName(),
                        this,
                        other
                ));
            }
        }
    }
}
