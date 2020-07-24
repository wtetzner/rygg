package org.bovinegenius.cmplang.ast.module;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NonNull;
import lombok.Value;

import java.util.Comparator;
import java.util.Objects;
import java.util.Optional;

@Value
@Builder
@AllArgsConstructor(staticName = "of")
public class Location implements Comparable<Location> {
    private static final Location EMPTY = Location.builder()
            .filename(Optional.empty())
            .line(1)
            .column(0)
            .build();

    @Builder.Default
    @NonNull Optional<String> filename = Optional.empty();
    @NonNull int line;
    @NonNull int column;

    public static Location of(String filename, int line, int column) {
        return Location.of(Optional.ofNullable(filename), line, column);
    }

    public static Location of(int line, int column) {
        return Location.of(Optional.empty(), line, column);
    }

    public static Location empty() {
        return EMPTY;
    }

    @Override
    public String toString() {
        if (this.filename.isPresent()) {
            return String.format("%s:%s:%s", this.filename.get(), this.line, this.column);
        } else {
            return String.format("%s:%s", this.line, this.column);
        }
    }

    @Override
    public int compareTo(Location o) {
        if (o == null) {
            return 1;
        }
        if (this.filename.isPresent() && o.filename.isPresent()) {
            int result = this.filename.get().compareTo(o.filename.get());
            if (result != 0) {
                return result;
            }
        }
        return Comparator.comparing(Location::getLine)
                .thenComparing(Location::getColumn)
                .compare(this, o);
    }

}
