package org.bovinegenius.cmplang.modules;

import lombok.NonNull;
import lombok.ToString;
import org.bovinegenius.cmplang.ast.module.Ident;
import org.bovinegenius.cmplang.ast.module.Subst;
import org.pcollections.HashTreePMap;
import org.pcollections.PMap;

import java.util.Map;
import java.util.Objects;
import java.util.Optional;

@ToString
public class IdentMap<LOC extends Comparable<LOC>, NAME, T extends Substable<LOC, NAME, T>> implements Substable<LOC, NAME, IdentMap<LOC, NAME, T>> {
    private final PMap<Ident<LOC, NAME>, T> identMap;
    private final PMap<NAME, T> nameMap;

    private IdentMap(
            final PMap<Ident<LOC, NAME>, T> identMap,
            final PMap<NAME, T> nameMap
    ) {
        this.identMap = identMap;
        this.nameMap = nameMap;
    }

    public static <LOC extends Comparable<LOC>, NAME, T extends Substable<LOC, NAME, T>> IdentMap<LOC, NAME, T> empty() {
        return new IdentMap(HashTreePMap.empty(), HashTreePMap.empty());
    }

    public boolean contains(@NonNull Ident<LOC, NAME> ident) {
        return this.identMap.containsKey(ident);
    }

    public boolean contains(@NonNull NAME name) {
        return this.nameMap.containsKey(name);
    }

    public Optional<Ident<LOC, NAME>> ident(@NonNull NAME name) {
        for (Ident<LOC, NAME> ident : identMap.keySet()) {
            if (Objects.equals(name, ident.getName())) {
                return Optional.of(ident);
            }
        }
        return Optional.empty();
    }

    public Optional<T> get(@NonNull Ident<LOC, NAME> ident) {
        return Optional.ofNullable(identMap.get(ident));
    }

    public Optional<T> get(@NonNull NAME name) {
        return Optional.ofNullable(nameMap.get(name));
    }

    public IdentMap<LOC, NAME, T> plus(@NonNull Ident<LOC, NAME> ident, T value) {
        return new IdentMap<>(this.identMap.plus(ident, value), this.nameMap.plus(ident.getName(), value));
    }

    public Iterable<Map.Entry<Ident<LOC, NAME>, T>> entries() {
        return this.identMap.entrySet();
    }

    @Override
    public IdentMap<LOC, NAME, T> subst(Subst<LOC, NAME> subst) {
        IdentMap<LOC, NAME, T> result = IdentMap.empty();
        for (Map.Entry<Ident<LOC, NAME>, T> entry : this.entries()) {
            result = result.plus(entry.getKey(), entry.getValue().subst(subst));
        }
        return result;
    }
}
