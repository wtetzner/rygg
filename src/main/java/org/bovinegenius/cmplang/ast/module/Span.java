package org.bovinegenius.cmplang.ast.module;

import lombok.AllArgsConstructor;
import lombok.NonNull;
import lombok.Value;

@Value
@AllArgsConstructor(staticName = "of")
public class Span implements Comparable<Span> {
    private static final Span EMPTY = Span.of(Location.empty(), Location.empty());

    @NonNull Location start;
    @NonNull Location end;

    public static Span of(Location location) {
        return Span.of(location, location);
    }

    public static Span empty() {
        return EMPTY;
    }

    @Override
    public int compareTo(Span o) {
        if (null == o) {
            return 1;
        }
        return this.getStart().compareTo(o.getStart());
    }

}
