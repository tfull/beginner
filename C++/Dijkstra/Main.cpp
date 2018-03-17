#include <iostream>
#include <vector>

int main(){
    const int infty = 1000000000;
    int n;
    int start, goal;
    int **edge;
    int *previous;
    int *distance;
    bool *vertex;

    std::cin >> n;
    std::cin >> start >> goal;

    edge = new int*[n];
    for(int i = 0; i < n; i++){
        edge[i] = new int[n];
        for(int j = 0; j < n; j++){
            edge[i][j] = -1;
        }
    }

    while(! std::cin.eof()){
        int i, j, v;
        std::cin >> i >> j >> v;
        edge[i][j] = v;
        edge[j][i] = v;
    }

    previous = new int[n];
    for(int i = 0; i < n; i++){
        previous[i] = -1;
    }

    distance = new int[n];
    for(int i = 0; i < n; i++){
        distance[i] = infty;
    }

    vertex = new bool[n];
    for(int i = 0; i < n; i++){
        vertex[i] = true;
    }

    distance[start] = 0;

    while(true){
        int index = -1;
        int value = infty;
        for(int i = 0; i < n; i++){
            if(vertex[i] && distance[i] < value){
                index = i;
                value = distance[i];
            }
        }
        if(index == -1){
            break;
        }
        vertex[index] = false;
        for(int i = 0; i < n; i++){
            if(i == index || edge[index][i] == -1){
                continue;
            }
            if(distance[i] > distance[index] + edge[index][i]){
                distance[i] = distance[index] + edge[index][i];
                previous[i] = index;
            }
        }
    }

    std::vector<int> route;
    for(int i = goal; i != -1; i = previous[i]){
        route.push_back(i);
    }
    for(int i = route.size() - 1; i >= 0; i--){
        std::cout << route[i];
        if(i > 0){
            std::cout << " -> ";
        }
    }
    std::cout << std::endl;
    if(route[route.size() - 1] == start){
        std::cout << "distance: " << distance[goal] << std::endl;
    }else{
        std::cout << "unreachable" << std::endl;
    }

    for(int i = 0; i < n; i++){
        delete[] edge[i];
    }
    delete[] edge;

    delete[] previous;
    delete[] distance;
    delete[] vertex;

    return 0;
}
