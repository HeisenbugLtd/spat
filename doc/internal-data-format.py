#!/usr/bin/env python3

"""
Creates a graphviz dotfile to show our internal data format
and renders it to a .png with the use of graphviz.
"""

from graphviz import Digraph

def repeat_anchors(up_to, name):
    """
    Repeats given name given up_to times with gv specific separators and (according to
    up_to) unique anchors. The resulting string is returned.
    """
    result = ""
    for i in up_to[:-1]:
        result = "".join([result, "<", str(i), ">", name, "|"])

    return "".join([result, "<", str(up_to[-1]), ">..."])

def main():
    """Main function"""
    __anchors_range = range(4)

    # Repeatedly used _attributes
    __top_bottom = {"rankdir":"TB"}
    __dashed = {"style":"dashed"}

    # Main graph
    graph = Digraph(name="Internal Data Format",
                    filename="internal-data-format",
                    format="png",
                    body=('\trankdir=LR',
                          '\tviewport="800,600,0.9"',
                          '\tordering=out',
                          '\tbgcolor="#EEEEEE"'),
                    node_attr={"shape":"record"})
    graph.node(name="Analyzed_Entity",
               label="Analyzed_Entity|{|{<t>The_Tree|<f>Flows|<p>Proofs}}")

    # The "root" of the entity tree
    with graph.subgraph(name="Tree_Root") as subgraph:
        subgraph.node(name="Root",
                      label="<t>The_Tree|<f>Flows_Sentinel|"\
                            "{|flow-data}|<p>Proofs_Sentinel|{|{proof-data}}|...")

    # The flow items. Subtree of Flows_Sentinel
    with graph.subgraph(name="Flow_Tree", graph_attr=__top_bottom) as subgraph:
        subgraph.node(name="Flow_Items",
                      label=repeat_anchors(up_to=__anchors_range, name="flow_item|{|{flow_data}}"))

    # The proof items. Subtree of Proofs_Sentinel
    with graph.subgraph(name="Proof_Tree", graph_attr=__top_bottom) as subgraph:
        subgraph.node(name="Proof_Items",
                      label="".join([repeat_anchors(up_to=__anchors_range,
                                                    name="proof_item|{|{proof_data}}")]))

    # Proof attempt subtrees, subtree of their respective Proof_Items
    for i in __anchors_range[:-1]:
        with graph.subgraph(name="".join(["Proof_Attempts_", str(i), "_N"]),
                            graph_attr=__top_bottom) as subgraph:
            subgraph.node(name="".join(["Proof_Attempts_", str(i)]),
                          label=repeat_anchors(up_to=__anchors_range, name="proof_attempt"))

    # Make the connections

    # Analyzed entity connections to tree
    graph.edge("Analyzed_Entity:t", "Root:t", _attributes={"arrowhead":"none"})
    graph.edge("Analyzed_Entity:f", "Root:f", _attributes=__dashed)
    graph.edge("Analyzed_Entity:p", "Root:p", _attributes=__dashed)

    # Flow root to each flow item
    for i in __anchors_range[:-1]:
        graph.edge("Root:f",
                   "".join(["Flow_Items:", str(i)]))

    # Last edge dashed
    graph.edge("Root:f",
               "".join(["Flow_Items:", str(__anchors_range[-1])]),
               _attributes=__dashed)

    # Proof root to each proof item.
    for i in __anchors_range[:-1]:
        graph.edge("Root:p",
                   "".join(["Proof_Items:", str(i)]))

    # Last edge dashed
    graph.edge("Root:p",
               "".join(["Proof_Items:", str(__anchors_range[-1])]),
               _attributes=__dashed)

    # Each proof item to their respective proof attempts.
    for i in __anchors_range[:-1]:
        # Undashed edges
        for j in __anchors_range[:-1]:
            graph.edge("".join(["Proof_Items:", str(i)]),
                       "".join(["Proof_Attempts_", str(i), ":", str(j)]))

        # Last edge dashed
        graph.edge("".join(["Proof_Items:", str(i)]),
                   "".join(["Proof_Attempts_", str(i), ":", str(__anchors_range[-1])]),
                   _attributes=__dashed)

    graph.render(view=False, cleanup=True)

if __name__ == "main":
    main()
