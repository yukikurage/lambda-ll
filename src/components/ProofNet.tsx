"use client";

import React, { useEffect, useRef } from "react";
import * as d3 from "d3";
import { Variables } from "../lambdaLL/typeChecker";
import { ContextTree, ContextId, getNode } from "../lambdaLL/contextTree";

type ProofNetProps = {
  tree: ContextTree;
  vars: Variables; // Top level vars
};

// Graph types
type NodeParams = {
  id: string; // contextId or varName
  label: string;
  typeLabel: string;
  x: number;
  y: number;
  kind: "var" | "tensor" | "par" | "root";
};
type LinkParams = {
  source: string;
  target: string;
  kind: "intro" | "elim" | "structural"; // Intro=convex up, Elim=convex down
};

export const ProofNet: React.FC<ProofNetProps> = ({ tree, vars }) => {
  const svgRef = useRef<SVGSVGElement>(null);

  // Data Transformation
  const graphData = React.useMemo(() => {
    if (!tree) return null;

    // This is tricky. AST is not passed here, only `vars` and `tree`.
    // `tree` contains the state of the Context.
    // A Proof Net is actually a history of deductions?
    // Or is it the structure of the Terms?
    // User requested:
    // "intro x, y -> convex up curve"
    // "elim t1, t2 -> convex down curve"
    // The `ContextTree` only represents the *Leaves* and *Branches* available at a specific moment (or final state).
    // It does NOT represent the history of `intro` / `elim`.
    // `intro` adds a `par` branch.
    // `elim` removes nodes.

    // To visualize "elim t1, t2", we need to know that `elim` happened.
    // `check_program` executes statements and modifies the tree.
    // If we want to visualize the *Proof*, we need the *AST* or a *Trace* of the proof.
    // The user said: "term t -> connect variables".
    // "intro x, y -> ... "
    // "let x = t -> ... "

    // So we need to visualize the *Statements*?
    // Actually, Proof Net usually represents the flow of the linear logic term.
    // If we look at `mp1` example:
    // It's a series of statements inside a block.
    // The block *builds* a term.

    // Suggestion: The *AST* is the recipe for the Proof Net.
    // `check_program` verifies it.
    // While verifying, we can build a Graph.
    // But here `ProofNet` component only receives `tree` and `vars`.
    // `vars` points to the resulting *Values* (Terms).

    // If I only have final `vars`, I can only show the final structure (Result Net?).
    // But user wants to see `intro` / `elim` curves.
    // This means I need to visualize the *Program Execution*.

    // Therefore, `ProofNet` needs the `Program` (AST) or `Trace`.
    // I should probably update `CodeEditor` to pass the `Program` AST to `ProofNet`.
    // Or even better, `check_program` can return a `ProofNetGraph`!

    // Let's stub this for now. I'll implement a simple tree visualizer of the current `ContextTree` state,
    // which represents the "Resulting Sequent".
    // And I will plan to extend `check_program` to generate the full net trace.

    // For now, let's visualize the `ContextTree` structure of the `vars`.
    // `vars` map string -> ContextId.
    // We can traverse from these IDs up to Root?
    // No, `vars` are leaves.
    // The tree starts at `root`.
    // We can traverse `tree` from Root down to Leaves.
    // Leaves are identified by IDs. `vars` give names to some IDs.

    const nodes: NodeParams[] = [];
    const links: LinkParams[] = [];

    const width = 600;

    // Simple tree layout
    // We can recursively traverse `tree.root`.

    const traverse = (
      id: ContextId,
      r: number,
      c: number,
      w: number,
      parentId?: string
    ) => {
      const node = getNode(tree, id);
      const nodeId = id.toString();

      // Check if this node is a var
      let label = "";
      let varName = "";
      for (const [v, vid] of vars.entries()) {
        if (vid === id) {
          varName = v;
          break;
        }
      }

      if (node.nodeType === "branch") {
        label = node.branchType === "tensor" ? "⊗" : "⅋";
      } else {
        label = varName || "?"; // Leaf
      }

      nodes.push({
        id: nodeId,
        label,
        typeLabel: node.nodeType === "leaf" ? JSON.stringify(node.type) : "", // Simplified type string
        x: c,
        y: r,
        kind: node.nodeType === "branch" ? node.branchType : "var",
      });

      if (parentId) {
        links.push({
          source: parentId,
          target: nodeId,
          kind: "structural",
        });
      }

      if (node.nodeType === "branch") {
        const step = w / node.children.length;
        node.children.forEach((childId, i) => {
          traverse(
            childId,
            r + 60,
            c - w / 2 + step * i + step / 2,
            w / 2,
            nodeId
          );
        });
      }
    };

    if (tree.root !== undefined) {
      traverse(tree.root, 50, width / 2, width, undefined);
    }

    return { nodes, links };
  }, [tree, vars]);

  // Rendering
  useEffect(() => {
    if (!graphData || !svgRef.current) return;

    const svg = d3.select(svgRef.current);
    svg.selectAll("*").remove();

    // Links
    svg
      .selectAll("path")
      .data(graphData.links)
      .enter()
      .append("path")
      .attr("d", (d) => {
        const source = graphData.nodes.find((n) => n.id === d.source);
        const target = graphData.nodes.find((n) => n.id === d.target);
        if (!source || !target) return "";

        const x1 = source.x;
        const y1 = source.y;
        const x2 = target.x;
        const y2 = target.y;
        const midY = (y1 + y2) / 2;

        return `M ${x1} ${y1} C ${x1} ${midY}, ${x2} ${midY}, ${x2} ${y2}`;
      })
      .attr("stroke", "#555")
      .attr("stroke-width", 2)
      .attr("fill", "none");

    // Nodes
    const nodeGroup = svg
      .selectAll("g")
      .data(graphData.nodes)
      .enter()
      .append("g")
      .attr("transform", (d) => `translate(${d.x},${d.y})`);

    nodeGroup
      .append("circle")
      .attr("r", 15)
      .attr("fill", (d) =>
        d.kind === "tensor"
          ? "#3b82f6"
          : d.kind === "par"
          ? "#a855f7"
          : "#eab308"
      )
      .attr("stroke", "#fff")
      .attr("stroke-width", 1);

    nodeGroup
      .append("text")
      .text((d) => d.label)
      .attr("text-anchor", "middle")
      .attr("dy", 5)
      .attr("fill", "white")
      .attr("font-size", "12px")
      .attr("font-weight", "bold");

    // Var names / types tooltip?
  }, [graphData]);

  return (
    <svg
      ref={svgRef}
      width="100%"
      height="400"
      className="border border-neutral-800 bg-neutral-950 rounded"
      style={{ minHeight: "400px" }}
    >
      {/* D3 will render here */}
    </svg>
  );
};
