#!/usr/bin/env python3

from graphviz import Digraph

def repeat_anchors (up_to, name):
    result = ""
    for i in up_to[:-1]:
        result = "".join ([result, "<", str (i), ">", name, "|"])

    return "".join ([result, "<", str(up_to[-1]), ">..."])

def main():
    ANCHORS_RANGE = range(4)

    # Repeatedly used _attributes
    TOP_BOTTOM = {"rankdir":"TB"}
    DASHED = {"style":"dashed"}

    # Main graph
    graph = Digraph (name      = "Internal Data Format",
                     filename  = "internal-data-format",
                     format    = "png",
                     body      = ('\trankdir=LR',
                                  '\tviewport="800,600,0.9"',
                                  '\tordering=out',
                                  '\tbgcolor="#EEEEEE"'),
                     node_attr = {"shape":"record"})
    graph.node (name  = "Analyzed_Entity",
                label = "Analyzed_Entity|{|{<t>The_Tree|<f>Flows|<p>Proofs}}")

    # The "root" of the entity tree
    with graph.subgraph (name = "Tree_Root") as s:
        s.node (name  = "Root",
                label = "<t>The_Tree|<f>Flows_Sentinel|{|flow-data}|<p>Proofs_Sentinel|{|{proof-data}}|...")

    # The flow items. Subtree of Flows_Sentinel
    with graph.subgraph (name = "Flow_Tree", graph_attr = TOP_BOTTOM) as s:
        s.node (name  = "Flow_Items",
                label = repeat_anchors (up_to = ANCHORS_RANGE, name = "flow_item|{|{flow_data}}"))

    # The proof items. Subtree of Proofs_Sentinel
    with graph.subgraph (name = "Proof_Tree", graph_attr = TOP_BOTTOM) as s:
        s.node (name = "Proof_Items",
                label = "".join ([repeat_anchors (up_to = ANCHORS_RANGE, name = "proof_item|{|{proof_data}}")]))

    # Proof attempt subtrees, subtree of their respective Proof_Items
    for x in ANCHORS_RANGE[:-1]:
        with graph.subgraph (name       = "".join (["Proof_Attempts_", str(x), "_N"]),
                             graph_attr = TOP_BOTTOM) as s:
            s.node (name = "".join (["Proof_Attempts_", str(x)]),
                    label = repeat_anchors (up_to = ANCHORS_RANGE, name = "proof_attempt"))

    # Make the connections

    # Analyzed entity connections to tree
    graph.edge ("Analyzed_Entity:t", "Root:t", _attributes={"arrowhead":"none"})
    graph.edge ("Analyzed_Entity:f", "Root:f", _attributes=DASHED)
    graph.edge ("Analyzed_Entity:p", "Root:p", _attributes=DASHED)

    # Flow root to each flow item
    for i in ANCHORS_RANGE[:-1]:
        graph.edge ("Root:f",
                    "".join (["Flow_Items:", str(i)]))

    # Last edge dashed
    graph.edge ("Root:f",
                "".join (["Flow_Items:", str(ANCHORS_RANGE[-1])]),
                _attributes=DASHED)

    # Proof root to each proof item.
    for i in ANCHORS_RANGE[:-1]:
        graph.edge ("Root:p",
                    "".join (["Proof_Items:", str(i)]))

    # Last edge dashed
    graph.edge ("Root:p",
                "".join (["Proof_Items:", str(ANCHORS_RANGE[-1])]),
                _attributes=DASHED)

    # Each proof item to their respective proof attempts.
    for x in ANCHORS_RANGE[:-1]:
        # Undashed edges
        for y in ANCHORS_RANGE[:-1]:
          graph.edge ("".join (["Proof_Items:", str(x)]),
                      "".join (["Proof_Attempts_", str(x), ":", str(y)]))

        # Last edge dashed
        graph.edge ("".join (["Proof_Items:", str(x)]),
                    "".join (["Proof_Attempts_", str(x), ":", str(ANCHORS_RANGE[-1])]),
                    _attributes=DASHED)

    graph.render(view = False, cleanup = True)

main()
