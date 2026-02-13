class Graph():
    def __init__(self, vertices):
        self.V = vertices
        self.graph = [[0 for column in range(vertices)]
                      for row in range(vertices)]

    def minDistance(self, dist, sptSet):
        """
        Find vertex with minimum distance value from the set of vertices
        not yet included in shortest path tree.
        
        Args:
            dist (list): Current distances from source to each vertex
            sptSet (list): Boolean array marking processed vertices
            
        Returns:
            int: Index of vertex with minimum distance, or -1 if none found
        """
        min_val = 1e7
        min_index = -1
        
        for v in range(self.V):
            if dist[v] < min_val and sptSet[v] == False:
                min_val = dist[v]
                min_index = v
        
        return min_index

    def dijkstraWithPath(self, src, dest):
        """
        Find shortest path from source to destination using Dijkstra's algorithm.
        
        Args:
            src (int): Source vertex
            dest (int): Destination vertex
            
        Returns:
            tuple: (distance, path) where distance is total cost and path is list of vertices,
                   or (None, []) if no path exists
        """

        dist = [1e7] * self.V
        dist[src] = 0
        
        sptSet = [False] * self.V
        
        parent = [-1] * self.V

        for cout in range(self.V):
            u = self.minDistance(dist, sptSet)
            
            if u == -1:
                break
            
            sptSet[u] = True

            if u == dest:
                break
            
            for v in range(self.V):
                if (self.graph[u][v] > 0 and 
                   sptSet[v] == False and 
                   dist[v] > dist[u] + self.graph[u][v]):
                    dist[v] = dist[u] + self.graph[u][v]
                    parent[v] = u  

        if dist[dest] == 1e7:
            return None, []
        
        path = []
        curr = dest
        while curr != -1:
            path.append(curr)
            curr = parent[curr]

        return dist[dest], path[::-1]


def find_shortest_path(start_node, end_node):
    """
    Find and display shortest path between two nodes.
    
    Args:
        start_node (int): Starting vertex
        end_node (int): Ending vertex
    """
    distance, path = g.dijkstraWithPath(start_node, end_node)
    
    if distance is None:
        print(f"Path from {start_node} to {end_node} does not exist.")
    else:
        print(f"Path: {' -> '.join(map(str, path))}")
        print(f"Total Distance: {distance}")