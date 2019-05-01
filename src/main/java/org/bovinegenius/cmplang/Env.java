package org.bovinegenius.cmplang;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

import org.bovinegenius.cmplang.ModuleType.Signature;
import org.bovinegenius.cmplang.Specification.ModuleSig;
import org.bovinegenius.cmplang.Specification.TypeSig;
import org.bovinegenius.cmplang.Specification.ValueSig;
import org.pcollections.HashPMap;
import org.pcollections.HashTreePMap;

public interface Env<NAME, IDENT extends Ident<NAME>, TERM, VAL, DEF, KIND> {
    public static enum EntryType {
        VALUE,
        TYPE,
        MODULE
    }

    public static enum NamespaceType {
        SINGLE,
        SEPARATE;
    }

    public static <NAME, IDENT extends Ident<NAME>, TERM, VAL, DEF, KIND> Env<NAME, IDENT, TERM, VAL, DEF, KIND> empty(CoreSyntax<NAME, IDENT, TERM, VAL, DEF, KIND> coreSyntax) {
        return Private.EnvImpl.of(coreSyntax);
    }

    public Env<NAME, IDENT, TERM, VAL, DEF, KIND> withValue(IDENT name, VAL value);
    public Env<NAME, IDENT, TERM, VAL, DEF, KIND> withType(IDENT name, TypeDecl<KIND, DEF> value);
    public Env<NAME, IDENT, TERM, VAL, DEF, KIND> withModule(IDENT name, ModuleType<NAME, IDENT, VAL, KIND, DEF> value);
    public VAL findValue(Path<NAME, IDENT> path);
    public TypeDecl<KIND, DEF> findType(Path<NAME, IDENT> path);
    public ModuleType<NAME, IDENT, VAL, KIND, DEF> findModule(Path<NAME, IDENT> path);
    public Env<NAME, IDENT, TERM, VAL, DEF, KIND> empty();

    default public Env<NAME, IDENT, TERM, VAL, DEF, KIND> withSpec(Specification<NAME, IDENT, VAL, KIND, DEF> value) {
        if (null == value) {
            throw new NullPointerException("value cannot be null");
        }
        if (value instanceof ValueSig) {
            ValueSig<NAME, IDENT, VAL, KIND, DEF> sig = (ValueSig<NAME, IDENT, VAL, KIND, DEF>)value;
            return this.withValue(sig.getName(), sig.getType());
        } else if (value instanceof TypeSig) {
            TypeSig<NAME, IDENT, VAL, KIND, DEF> sig = (TypeSig<NAME, IDENT, VAL, KIND, DEF>)value;
            return this.withType(sig.getName(), sig.getDecl());
        } else if (value instanceof ModuleSig) {
            ModuleSig<NAME, IDENT, VAL, KIND, DEF> sig = (ModuleSig<NAME, IDENT, VAL, KIND, DEF>)value;
            return this.withModule(sig.getName(), sig.getType());
        } else {
            throw new IllegalArgumentException(String.format("Unknown sig item type (%s): %s", value.getClass().getCanonicalName(), value));
        }
    }

    default public Env<NAME, IDENT, TERM, VAL, DEF, KIND> withSignature(Signature<NAME, IDENT, VAL, KIND, DEF> value) {
        if (null == value) {
            throw new NullPointerException("value cannot be null");
        }
        Signature<NAME, IDENT, VAL, KIND, DEF> sig = (Signature<NAME, IDENT, VAL, KIND, DEF>)value;
        Env<NAME, IDENT, TERM, VAL, DEF, KIND> result = this;
        for (Specification<NAME, IDENT, VAL, KIND, DEF> spec : sig.getSpecifications()) {
            result = result.withSpec(spec);
        }
        return result;
    }

    public static class Private {
        private static class EnvImpl<NAME, IDENT extends Ident<NAME>, TERM, VAL, DEF, KIND> implements Env<NAME, IDENT, TERM, VAL, DEF, KIND> {
            private final HashPMap<IDENT, VAL> values;
            private final HashPMap<IDENT, TypeDecl<KIND, DEF>> types;
            private final HashPMap<IDENT, ModuleType<NAME, IDENT, VAL, KIND, DEF>> modules;
            private final CoreSyntax<NAME, IDENT, TERM, VAL, DEF, KIND> coreSyntax;

            public EnvImpl(
                    HashPMap<IDENT, VAL> values,
                    HashPMap<IDENT, TypeDecl<KIND, DEF>> types,
                    HashPMap<IDENT, ModuleType<NAME, IDENT, VAL, KIND, DEF>> modules,
                    CoreSyntax<NAME, IDENT, TERM, VAL, DEF, KIND> coreSyntax
                    ) {
                this.values = values;
                this.types = types;
                this.modules = modules;
                this.coreSyntax = coreSyntax;
            }

            private static <NAME, IDENT extends Ident<NAME>, TERM, VAL, DEF, KIND> Env<NAME, IDENT, TERM, VAL, DEF, KIND> of(CoreSyntax<NAME, IDENT, TERM, VAL, DEF, KIND> coreSyntax) {
                return new EnvImpl<>(HashTreePMap.empty(), HashTreePMap.empty(), HashTreePMap.empty(), coreSyntax);
            }

            public Env<NAME, IDENT, TERM, VAL, DEF, KIND> empty() {
                return new EnvImpl<>(HashTreePMap.empty(), HashTreePMap.empty(), HashTreePMap.empty(), coreSyntax);
            }

            @Override
            public Env<NAME, IDENT, TERM, VAL, DEF, KIND> withValue(IDENT name, VAL value) {
                return new EnvImpl<>(values.plus(name,  value), types, modules, coreSyntax);
            }

            @Override
            public Env<NAME, IDENT, TERM, VAL, DEF, KIND> withType(IDENT name, TypeDecl<KIND, DEF> value) {
                return new EnvImpl<>(values, types.plus(name, value), modules, coreSyntax);
            }

            @Override
            public Env<NAME, IDENT, TERM, VAL, DEF, KIND> withModule(IDENT name, ModuleType<NAME, IDENT, VAL, KIND, DEF> value) {
                return new EnvImpl<>(values, types, modules.plus(name, value), coreSyntax);
            }

            private Specification<NAME, IDENT, VAL, KIND, DEF> findField(Path<NAME, IDENT> path, Subst<NAME, IDENT> subst, List<Specification<NAME, IDENT, VAL, KIND, DEF>> specs) {
                NAME tail = path.getTail().orElse(null);
                return specs.stream()
                        .filter(spec -> spec.getName().equals(tail))
                        .findFirst()
                        .orElseThrow(() -> new RuntimeException(String.format("No field found for %s", path)));
            }

            private <T> T findSpec(Map<IDENT, T> map, Path<NAME, IDENT> path, Function<Specification<NAME, IDENT, VAL, KIND, DEF>, T> extract) {
                if (path.getComponents().isEmpty()) {
                    return map.get(path.getIdent());
                }
                ModuleType<NAME, IDENT, VAL, KIND, DEF> module = findModule(path.getRoot());
                if (null == module) {
                    throw new RuntimeException(String.format("No module found for %s", path.getRoot()));
                }
                if (module instanceof Signature) {
                    Signature<NAME, IDENT, VAL, KIND, DEF> sig = (Signature<NAME, IDENT, VAL, KIND, DEF>)module;
                    Specification<NAME, IDENT, VAL, KIND, DEF> field = findField(path, Subst.identity(), sig.getSpecifications());
                    T result = extract.apply(field);
                    if (null == result) {
                        throw new RuntimeException(String.format("No field found for %s", path));
                    }
                    return result;
                } else {
                    throw new RuntimeException(String.format("Expected Signature, found (%s) %s.", module.getClass().getCanonicalName(), module));
                }
            }

            @Override
            public VAL findValue(Path<NAME, IDENT> path) {
                return findSpec(values, path, field -> {
                    if (field instanceof ValueSig) {
                        ValueSig<NAME, IDENT, VAL, KIND, DEF> valueSig = (ValueSig<NAME, IDENT, VAL, KIND, DEF>)field;
                        return valueSig.getType();
                    } else {
                        return null;
                    }
                });
            }

            @Override
            public TypeDecl<KIND, DEF> findType(Path<NAME, IDENT> path) {
                return findSpec(types, path, field -> {
                    if (field instanceof ValueSig) {
                        TypeSig<NAME, IDENT, VAL, KIND, DEF> sig = (TypeSig<NAME, IDENT, VAL, KIND, DEF>)field;
                        return sig.getDecl();
                    } else {
                        return null;
                    }
                });
            }

            @Override
            public ModuleType<NAME, IDENT, VAL, KIND, DEF> findModule(Path<NAME, IDENT> path) {
                return findSpec(modules, path, field -> {
                    if (field instanceof ValueSig) {
                        ModuleSig<NAME, IDENT, VAL, KIND, DEF> sig = (ModuleSig<NAME, IDENT, VAL, KIND, DEF>)field;
                        return sig.getType();
                    } else {
                        return null;
                    }
                });
            }
        }
    }
}
