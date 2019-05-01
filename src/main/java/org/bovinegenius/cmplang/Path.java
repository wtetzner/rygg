package org.bovinegenius.cmplang;

import java.util.Objects;
import java.util.Optional;

import org.pcollections.PVector;
import org.pcollections.TreePVector;

public class Path<NAME, T extends Ident<NAME>> {
    private final T ident;
    private final PVector<NAME> components;

    private Path(T ident, PVector<NAME> components) {
        if (null == ident) {
            throw new NullPointerException("ident cannot be null");
        }
        this.ident = ident;
        this.components = (components == null) ? TreePVector.empty() : components;
    }

    public T getIdent() {
        return this.ident;
    }

    public PVector<NAME> getComponents() {
        return this.components;
    }

    public Optional<NAME> getTail() {
        if (!components.isEmpty()) {
            return Optional.ofNullable(components.get(components.size() - 1));
        }
        return Optional.empty();
    }

    public Path<NAME, T> getRoot() {
        if (components.size() <= 1) {
            return new Path<>(ident, TreePVector.empty());
        } else {
            PVector<NAME> newComps = TreePVector.empty();
            for (int i = 0; i < components.size() - 1; i++) {
                newComps = newComps.plus(components.get(i));
            }
            return new Path<>(ident, newComps);
        }
    }

    public static <NAME, T extends Ident<NAME>> Path<NAME, T> of(T ident) {
        if (null == ident) {
            throw new NullPointerException("ident cannot be null");
        }
        return new Path<NAME, T>(ident, null);
    }

    public static <NAME, T extends Ident<NAME>> Path<NAME, T> of(T ident, PVector<NAME> components) {
        if (null == ident) {
            throw new NullPointerException("ident cannot be null");
        }
        return new Path<NAME, T>(ident, components);
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.ident, this.components);
    }

    @SuppressWarnings("rawtypes")
    @Override
    public boolean equals(Object other) {
        if (null == other) {
            return false;
        }
        if (other instanceof Path) {
            return Objects.equals(this.ident, ((Path) other).ident)
                    && Objects.equals(this.components, ((Path) other).components);
        }
        throw new IllegalArgumentException(String.format("Cannot compare object of type %s against object of type %s: %s vs %s",
                this.getClass().getCanonicalName(),
                other.getClass().getCanonicalName(),
                this,
                other
                ));
    }
}
