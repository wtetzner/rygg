package org.bovinegenius.cmplang.ast.module;

import org.bovinegenius.cmplang.ast.module.ModuleType.Signature;
import org.bovinegenius.cmplang.ast.module.Specification.ModuleSig;
import org.bovinegenius.cmplang.ast.module.Specification.TypeSig;
import org.bovinegenius.cmplang.ast.module.Specification.ValueSig;
import org.pcollections.HashPMap;
import org.pcollections.HashTreePMap;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

public interface Env<LOC, NAME, IDENT extends Ident<NAME,LOC>, TERM, VAL, DEF, KIND> {
    public static enum EntryType {
        VALUE,
        TYPE,
        MODULE
    }

    public static enum NamespaceType {
        SINGLE,
        SEPARATE;
    }

    public static <LOC,NAME, IDENT extends Ident<NAME,LOC>, TERM, VAL, DEF, KIND> Env<LOC,NAME, IDENT, TERM, VAL, DEF, KIND> empty(CoreSyntax<LOC,NAME, IDENT, TERM, VAL, DEF, KIND> coreSyntax) {
        return Private.EnvImpl.of(coreSyntax);
    }

    public Env<LOC,NAME, IDENT, TERM, VAL, DEF, KIND> withValue(IDENT name, VAL value);
    public Env<LOC,NAME, IDENT, TERM, VAL, DEF, KIND> withType(IDENT name, TypeDecl<KIND, DEF> value);
    public Env<LOC, NAME, IDENT, TERM, VAL, DEF, KIND> withModule(IDENT name, ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> value);
    public Optional<VAL> findValue(Path<LOC, NAME, IDENT> path);
    public Optional<TypeDecl<KIND, DEF>> findType(Path<LOC, NAME, IDENT> path);
    public Optional<ModuleType<LOC,NAME, IDENT, VAL, KIND, DEF>> findModule(Path<LOC, NAME, IDENT> path);
    public Env<LOC,NAME, IDENT, TERM, VAL, DEF, KIND> empty();

    default public Env<LOC,NAME, IDENT, TERM, VAL, DEF, KIND> withSpec(Specification<LOC, NAME, IDENT, VAL, KIND, DEF> value) {
        if (null == value) {
            throw new NullPointerException("value cannot be null");
        }
        if (value instanceof ValueSig) {
            ValueSig<LOC, NAME, IDENT, VAL, KIND, DEF> sig = (ValueSig<LOC, NAME, IDENT, VAL, KIND, DEF>)value;
            return this.withValue(sig.getName(), sig.getType());
        } else if (value instanceof TypeSig) {
            TypeSig<LOC, NAME, IDENT, VAL, KIND, DEF> sig = (TypeSig<LOC, NAME, IDENT, VAL, KIND, DEF>)value;
            return this.withType(sig.getName(), sig.getDecl());
        } else if (value instanceof ModuleSig) {
            ModuleSig<LOC, NAME, IDENT, VAL, KIND, DEF> sig = (ModuleSig<LOC, NAME, IDENT, VAL, KIND, DEF>)value;
            return this.withModule(sig.getName(), sig.getType());
        } else {
            throw new IllegalArgumentException(String.format("Unknown sig item type (%s): %s", value.getClass().getCanonicalName(), value));
        }
    }

    default public Env<LOC, NAME, IDENT, TERM, VAL, DEF, KIND> withSignature(Signature<LOC,NAME, IDENT, VAL, KIND, DEF> value) {
        if (null == value) {
            throw new NullPointerException("value cannot be null");
        }
        Signature<LOC,NAME, IDENT, VAL, KIND, DEF> sig = (Signature<LOC,NAME, IDENT, VAL, KIND, DEF>)value;
        Env<LOC,NAME, IDENT, TERM, VAL, DEF, KIND> result = this;
        for (Specification<LOC, NAME, IDENT, VAL, KIND, DEF> spec : sig.getSpecifications()) {
            result = result.withSpec(spec);
        }
        return result;
    }

    public static class Private {
        private static class EnvImpl<LOC,NAME, IDENT extends Ident<NAME,LOC>, TERM, VAL, DEF, KIND> implements Env<LOC,NAME, IDENT, TERM, VAL, DEF, KIND> {
            private final HashPMap<IDENT, VAL> values;
            private final HashPMap<IDENT, TypeDecl<KIND, DEF>> types;
            private final HashPMap<IDENT, ModuleType<LOC,NAME, IDENT, VAL, KIND, DEF>> modules;
            private final CoreSyntax<LOC,NAME, IDENT, TERM, VAL, DEF, KIND> coreSyntax;

            public EnvImpl(
                    HashPMap<IDENT, VAL> values,
                    HashPMap<IDENT, TypeDecl<KIND, DEF>> types,
                    HashPMap<IDENT, ModuleType<LOC,NAME, IDENT, VAL, KIND, DEF>> modules,
                    CoreSyntax<LOC,NAME, IDENT, TERM, VAL, DEF, KIND> coreSyntax
                    ) {
                this.values = values;
                this.types = types;
                this.modules = modules;
                this.coreSyntax = coreSyntax;
            }

            private static <LOC, NAME, IDENT extends Ident<NAME,LOC>, TERM, VAL, DEF, KIND> Env<LOC,NAME, IDENT, TERM, VAL, DEF, KIND> of(CoreSyntax<LOC, NAME, IDENT, TERM, VAL, DEF, KIND> coreSyntax) {
                return new EnvImpl<>(HashTreePMap.empty(), HashTreePMap.empty(), HashTreePMap.empty(), coreSyntax);
            }

            public Env<LOC, NAME, IDENT, TERM, VAL, DEF, KIND> empty() {
                return new EnvImpl<>(HashTreePMap.empty(), HashTreePMap.empty(), HashTreePMap.empty(), coreSyntax);
            }

            @Override
            public Env<LOC, NAME, IDENT, TERM, VAL, DEF, KIND> withValue(IDENT name, VAL value) {
                return new EnvImpl<>(values.plus(name,  value), types, modules, coreSyntax);
            }

            @Override
            public Env<LOC, NAME, IDENT, TERM, VAL, DEF, KIND> withType(IDENT name, TypeDecl<KIND, DEF> value) {
                return new EnvImpl<>(values, types.plus(name, value), modules, coreSyntax);
            }

            @Override
            public Env<LOC, NAME, IDENT, TERM, VAL, DEF, KIND> withModule(IDENT name, ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF> value) {
                return new EnvImpl<>(values, types, modules.plus(name, value), coreSyntax);
            }

            private Specification<LOC, NAME, IDENT, VAL, KIND, DEF> findField(Path<LOC, NAME, IDENT> path, Subst<LOC, NAME, IDENT> subst, List<Specification<LOC, NAME, IDENT, VAL, KIND, DEF>> specs) {
                NAME tail = path.getTail().orElse(null);
                return specs.stream()
                        .filter(spec -> spec.getName().equals(tail))
                        .findFirst()
                        .orElseThrow(() -> new RuntimeException(String.format("No field found for %s", path)));
            }

            private <T> Optional<T> findSpec(Map<IDENT, T> map, Path<LOC, NAME, IDENT> path, Function<Specification<LOC, NAME, IDENT, VAL, KIND, DEF>, T> extract) {
                if (path.getComponents().isEmpty()) {
                    return Optional.of(map.get(path.getIdent()));
                }
                Optional<ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF>> module = findModule(path.getRoot());
                if (null == module || !module.isPresent()) {
                    return Optional.empty();
                }
                if (module.get() instanceof Signature) {
                    Signature<LOC, NAME, IDENT, VAL, KIND, DEF> sig = (Signature<LOC, NAME, IDENT, VAL, KIND, DEF>)module.get();
                    Specification<LOC, NAME, IDENT, VAL, KIND, DEF> field = findField(path, Subst.identity(), sig.getSpecifications());
                    T result = extract.apply(field);
                    if (null == result) {
                        return Optional.empty();
                    }
                    return Optional.of(result);
                } else {
                    throw new RuntimeException(String.format("Expected Signature, found (%s) %s.", module.getClass().getCanonicalName(), module));
                }
            }

            @Override
            public Optional<VAL> findValue(Path<LOC, NAME, IDENT> path) {
                return findSpec(values, path, field -> {
                    if (field instanceof ValueSig) {
                        ValueSig<LOC, NAME, IDENT, VAL, KIND, DEF> valueSig = (ValueSig<LOC, NAME, IDENT, VAL, KIND, DEF>)field;
                        return valueSig.getType();
                    } else {
                        return null;
                    }
                });
            }

            @Override
            public Optional<TypeDecl< KIND, DEF>> findType(Path<LOC, NAME, IDENT> path) {
                return findSpec(types, path, field -> {
                    if (field instanceof ValueSig) {
                        TypeSig<LOC, NAME, IDENT, VAL, KIND, DEF> sig = (TypeSig<LOC, NAME, IDENT, VAL, KIND, DEF>)field;
                        return sig.getDecl();
                    } else {
                        return null;
                    }
                });
            }

            @Override
            public Optional<ModuleType<LOC, NAME, IDENT, VAL, KIND, DEF>> findModule(Path<LOC, NAME, IDENT> path) {
                return findSpec(modules, path, field -> {
                    if (field instanceof ValueSig) {
                        ModuleSig<LOC, NAME, IDENT, VAL, KIND, DEF> sig = (ModuleSig<LOC, NAME, IDENT, VAL, KIND, DEF>)field;
                        return sig.getType();
                    } else {
                        return null;
                    }
                });
            }
        }
    }
}
