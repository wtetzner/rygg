package org.bovinegenius.cmplang.modules;

import lombok.Value;

@Value(staticConstructor = "of")
public class NodeID<NODE> {
    private final long id;
}
