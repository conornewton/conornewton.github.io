---
title: Maze Generation
description: A maze generator using depth first search in typescript.
---

# Maze Generation

This maze generator was written in typescript, a language that compiles to javascript. The source code can be found on github [here](https://github.com/conornewton/maze-generation-typescript).

<body bgcolor="gray">
        <canvas id="cnvs" height="656" width="656"></canvas>
        <script>
var tile_size = 16;
var Edge = /** @class */ (function () {
    function Edge(left, right) {
        this.left = left;
        this.right = right;
    }
    return Edge;
}());
//i can just use an array of pairs of connections.
var Maze = /** @class */ (function () {
    function Maze(width, height) {
        var _this = this;
        this.generateMaze = function () {
            var visited = new Array(_this.width * _this.height);
            var stack = new Array();
            for (var i = 0; i < visited.length; i++) {
                visited[i] = false;
            }
            var current_cell = 0;
            visited[current_cell] = true;
            while (!visited.every(function (x) { return x == true; })) {
                //get the neighbours and filter out the visited ones.
                var neighbours = _this.getNeighbours(current_cell).filter(function (x) { return !visited[x]; });
                if (neighbours.length > 0) {
                    var chosen_neighbour = choose_random(neighbours);
                    stack.push(current_cell);
                    //link current_cell and chosen_neighbour together
                    _this.edges.push(new Edge(current_cell, chosen_neighbour));
                    current_cell = chosen_neighbour;
                    visited[current_cell] = true;
                }
                else {
                    current_cell = stack.pop();
                }
            }
        };
        //TODO: work out a quicker way of doing this.
        this.getNeighbours = function (index) {
            var x = index % _this.width;
            var y = (index - x) / _this.width;
            var neighbours = new Array();
            if (x > 0) {
                neighbours.push((x - 1) + y * _this.width);
            }
            if (y > 0) {
                neighbours.push(x + (y - 1) * _this.width);
            }
            if (x < _this.width - 1) {
                neighbours.push((x + 1) + y * _this.width);
            }
            if (y < _this.width - 1) {
                neighbours.push(x + (y + 1) * _this.width);
            }
            return neighbours;
        };
        this.draw = function (ctx) {
            //clear the screen
            ctx.fillStyle = "black";
            ctx.fillRect(0, 0, 656, 656);
            ctx.fillStyle = "white";
            for (var i = 0; i < _this.edges.length; i++) {
                var x1 = _this.edges[i].left % _this.width;
                var y1 = (_this.edges[i].left - x1) / _this.width;
                ctx.fillRect(2 * x1 * tile_size + tile_size, 2 * y1 * tile_size + tile_size, tile_size, tile_size);
                var x2 = _this.edges[i].right % _this.width;
                var y2 = (_this.edges[i].right - x2) / _this.width;
                ctx.fillRect(2 * x2 * tile_size + tile_size, 2 * y2 * tile_size + tile_size, tile_size, tile_size);
                ctx.fillRect((x1 + x2) * tile_size + tile_size, (y1 + y2) * tile_size + tile_size, tile_size, tile_size);
            }
        };
        this.width = width;
        this.height = height;
        this.edges = new Array();
        this.generateMaze();
    }
    return Maze;
}());
function choose_random(choices) {
    var index = Math.floor(Math.random() * choices.length);
    return choices[index];
}
window.onload = function () {
    var canvas = document.getElementById("cnvs");
    var ctx = canvas.getContext("2d");
    var width = (canvas.width - tile_size) / (2 * tile_size);
    var height = (canvas.height - tile_size) / (2 * tile_size);
    var maze = new Maze(width, height);
maze.draw(ctx);
}
        </script>
</body>
