package org.bovinegenius.cmplang.ast.module;

import java.util.Objects;
import java.util.Optional;

import org.bovinegenius.cmplang.util.Formatted;
import org.pcollections.PVector;
import org.pcollections.TreePVector;

public class Path<LOC, NAME> implements Formatted {
    private final Ident<LOC, NAME> ident;
    private final PVector<NAME> components;

    private Path(Ident<LOC, NAME> ident, PVector<NAME> components) {
        if (null == ident) {
            throw new NullPointerException("ident cannot be null");
        }
        this.ident = ident;
        this.components = (components == null) ? TreePVector.empty() : components;
    }

    public Ident<LOC, NAME> getIdent() {
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

    public Path<LOC, NAME> getRoot() {
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

    public Path<LOC, NAME> plus(NAME name) {
        return new Path(this.ident, this.components.plus(name));
    }

    public static <LOC, NAME> Path<LOC, NAME> of(Ident<LOC, NAME> ident) {
        if (null == ident) {
            throw new NullPointerException("ident cannot be null");
        }
        return new Path<LOC, NAME>(ident, null);
    }

    public static <LOC, NAME> Path<LOC, NAME> of(Ident<LOC, NAME> ident, PVector<NAME> components) {
        if (null == ident) {
            throw new NullPointerException("ident cannot be null");
        }
        return new Path<>(ident, components);
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

    @Override
    public String toString() {
        return this.formatted(true, 0, 0);
    }

    @Override
    public String formatted(boolean inline, int indentAmount, int indentLevel) {
        StringBuilder sb  = new StringBuilder();
        sb.append(this.getIdent());
        for (NAME name : this.getComponents()) {
            sb.append(".").append(name);
        }
        return sb.toString();
    }
}
