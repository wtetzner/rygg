
module type NODE_ID = sig
  type t
  val compare : t -> t -> int
end

module type NODE_BODY = sig
  type t
end

module type NODE = sig
  type t
  type body
  type id
  val id : t -> id
  val body : t -> body
  val map : (body -> body) -> t -> t
  val compare_id : id -> id -> int
end

module MakeNode(NodeId: NODE_ID)(NodeBody: NODE_BODY): NODE = struct
  type id = NodeId.t
  type body = NodeBody.t
  type t = {
      id: NodeId.t;
      body: NodeBody.t;
    }

  let id node = node.id
  let body node = node.body
  let map func node = { node with body = func node.body }
  let compare_id = NodeId.compare
end

module Edges(Node: NODE) : sig
  type t
  val assoc_id : t -> Node.id -> Node.id -> t
  val dissoc : t -> Node.id -> Node.id -> t
end = struct
  module NodeId = struct
    type t = Node.id
    let compare = Node.compare_id
  end
  module NodeMap = Map.Make(NodeId)
  module NodeSet = Set.Make(NodeId)

  type t = {
      to_nodes: NodeSet.t NodeMap.t;
      from_nodes: NodeSet.t NodeMap.t
    }

  let assoc_id edges from_node to_node =
    let to_set = match (NodeMap.find_opt from_node edges.to_nodes) with
      | None -> NodeSet.singleton to_node
      | Some set -> NodeSet.add to_node set in
    let from_set = match (NodeMap.find_opt to_node edges.from_nodes) with
      | None -> NodeSet.singleton from_node
      | Some set -> NodeSet.add from_node set in
    { edges with
      to_nodes = NodeMap.add
                   from_node
                   to_set
                   edges.to_nodes;
      from_nodes = NodeMap.add
                   to_node
                   from_set
                   edges.from_nodes;
    }

  let dissoc edges from_node to_node =
    let without_to =
      NodeMap.update from_node
        (function
         | None -> None
         | Some set ->
            let new_set = NodeSet.remove to_node set in
            if NodeSet.is_empty new_set then
              None
            else
              Some new_set)
        edges
    in
    NodeMap.update to_node
      (function
       | None -> None
       | Some set ->
          let new_set = NodeSet.remove from_node set in
          if NodeSet.is_empty new_set then
            None
          else
            Some new_set)
      without_to
end

module Make(Node: NODE): sig
  module Node : NODE
  type node
  type node_id
  type node_body
  type t
  val assoc : t -> node -> node -> t
  val dissoc : t -> node_id -> node_id -> t
  val map : t -> (node_body -> node_body) -> t
  val map_node : t -> node_id -> (node_body -> node_body) -> t
  val node_id : node -> node_id
  val node_body : node -> node_body
  val node : t -> node_id -> node option
end = struct
  module Node = Node
  module NId = struct
    type t = Node.id
    let compare = Node.compare_id
  end
  module NodeMap = Map.Make(NId)
  module NodeEdges = Edges(Node)

  type node = Node.t
  type node_id = Node.id
  type node_body = Node.body

  type t = {
      nodes: node NodeMap.t;
      edges: NodeEdges.t
    }

  let assoc graph from_node to_node =
    let from_id = Node.id from_node in
    let to_id = Node.id to_node in
    { graph with
      nodes = (NodeMap.add to_id to_node
                 (NodeMap.add from_id from_node graph.nodes));
      edges = NodeEdges.assoc_id graph.edges from_id to_id
    }

  let dissoc graph from_node to_node =
    { graph with
      edges = NodeEdges.dissoc graph.edges from_node to_node
    }

  let map graph func =
    { graph with
      nodes = NodeMap.map (fun node -> Node.map func node) graph.nodes
    }

  let map_node graph id func =
    if NodeMap.mem id graph.nodes then
      { graph with
        nodes = NodeMap.add id (Node.map func (NodeMap.find id graph.nodes)) graph.nodes
      }
    else
      graph

  let node_id node = Node.id node
  let node_body node = Node.body node
  let node graph id = NodeMap.find_opt id graph.nodes
end
