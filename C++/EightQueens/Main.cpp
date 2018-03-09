#include <iostream>

#define SIZE 8

char **read(){
    char **board;
    board = new char*[SIZE];

    for(int i = 0; i < SIZE; i++){
        board[i] = new char[SIZE];
        for(int j = 0; j < SIZE; j++){
            std::cin >> board[i][j];
        }
    }

    return board;
}

bool inRange(int i, int j){
    return i >= 0 && i < SIZE && j >= 0 && j < SIZE;
}

int countQueens(char **board){
    int count = 0;

    for(int i = 0; i < SIZE; i++){
        for(int j = 0; j < SIZE; j++){
            if(board[i][j] == 'Q'){
                count++;
            }
        }
    }
    return count;
}

bool check(char **board){
    for(int i = 0; i < SIZE; i++){
        for(int j = 0; j < SIZE; j++){
            if(board[i][j] == 'Q'){
                for(int di = -1; di <= 1; di++){
                    for(int dj = -1; dj <= 1; dj++){
                        if(di == 0 && dj == 0){
                            continue;
                        }
                        int ni = i + di;
                        int nj = j + dj;
                        while(inRange(ni, nj)){
                            if(board[ni][nj] == 'Q'){
                                return false;
                            }
                            ni += di;
                            nj += dj;
                        }
                    }
                }
            }
        }
    }
    return true;
}

bool search(char **board){
    if(check(board)){
        if(countQueens(board) == SIZE){
            return true;
        }
        for(int i = 0; i < SIZE; i++){
            for(int j = 0; j < SIZE; j++){
                if(board[i][j] != 'Q'){
                    board[i][j] = 'Q';
                    if(search(board)){
                        return true;
                    }
                    board[i][j] = '.';
                }
            }
        }
    }
    return false;
}

int main(){
    char **board = read();
    if(search(board)){
        for(int i = 0; i < SIZE; i++){
            for(int j = 0; j < SIZE; j++){
                std::cout << board[i][j];
            }
            std::cout << std::endl;
        }
    }else{
        std::cout << "impossible" << std::endl;
    }
    return 0;
}
