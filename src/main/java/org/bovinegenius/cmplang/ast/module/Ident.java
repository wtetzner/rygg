package org.bovinegenius.cmplang.ast.module;

import lombok.Getter;
import org.apache.commons.lang3.StringUtils;
import org.bovinegenius.cmplang.util.Display;
import org.bovinegenius.cmplang.util.Formatted;

import java.util.Objects;
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
public class Ident<LOC extends Comparable<LOC>, NAME> implements Display, Formatted, Comparable<Ident<LOC, NAME>> {
    private static final AtomicLong STAMP_SEQ = new AtomicLong(1);

    @Getter private final LOC location;
    private final NAME name;
    private final long stamp;

    private Ident(LOC location, NAME name, long stamp) {
        this.location = location;
        this.name = name;
        this.stamp = stamp;
    }

    public NAME getName() {
        return this.name;
    }

    public static <LOC extends Comparable<LOC>, NAME> Ident<LOC, NAME> create(LOC location, NAME name) {
        return new Ident<>(location, name, STAMP_SEQ.getAndIncrement());
    }

    @Override
    public int hashCode() {
        return Long.hashCode(this.stamp);
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean equals(Object other) {
        if (other == null) {
            return false;
        }
        if (other instanceof Ident) {
            return this.stamp == ((Ident<LOC, NAME>) other).stamp;
        }
        throw new IllegalArgumentException(String.format("Cannot compare object of type %s against object of type %s: %s vs %s",
                this.getClass().getCanonicalName(),
                other.getClass().getCanonicalName(),
                this,
                other
        ));
    }

    @Override
    public String toString() {
        String location = Objects.toString(this.location, "");
        if (!StringUtils.isBlank(location)) {
            return String.format("%s^%s:%s", this.name, this.stamp, location);
        } else {
            return String.format("%s^%s", this.name, this.stamp);
        }
    }

    @Override
    public String display() {
        return Objects.toString(this.name, "");
    }

    @Override
    public String formatted(boolean inline, int indentAmount, int indentLevel) {
        return Objects.toString(this.name, "");
    }

    @Override
    public int compareTo(Ident<LOC, NAME> o) {
        if (null == o) {
            return 1;
        }
        return this.getLocation().compareTo(o.getLocation());
    }

}

