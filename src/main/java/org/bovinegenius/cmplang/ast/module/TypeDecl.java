package org.bovinegenius.cmplang.ast.module;

import java.util.Optional;

public class TypeDecl<KIND, DEF> {
    private final KIND kind;
    private final Optional<DEF> manifest;

    private TypeDecl(KIND kind, Optional<DEF> manifest) {
        this.kind = kind;
        this.manifest = manifest;
    }

    public KIND getKind() {
        return this.kind;
    }

    public Optional<DEF> getManifest() {
        return this.manifest;
    }

    public static <KIND, DEF> TypeDecl<KIND, DEF> of(KIND kind, Optional<DEF> manifest) {
        return new TypeDecl<>(kind, manifest);
    }

    public static <KIND, DEF> TypeDecl<KIND, DEF> of(KIND kind, DEF manifest) {
        return new TypeDecl<>(kind, Optional.of(manifest));
    }

    public static <KIND, DEF> TypeDecl<KIND, DEF> of(KIND kind) {
        return new TypeDecl<>(kind, Optional.empty());
    }
}
