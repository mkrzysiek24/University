#include <iostream>
#include <limits>
#include <cmath>
#include <algorithm>

using namespace std;

bool sort_by_second(const pair<int, int> &a, const pair<int, int> &b) {
    return a.second < b.second;
}

double distance(pair<int, int> a, pair<int, int> b) {
    return sqrt(pow(b.first - a.first, 2) + pow(b.second - a.second, 2));
}

double brute_force(int start, int end, pair<int, int>* points, pair<int, int>* best_points) {
    double min_perimeter = numeric_limits<double>::infinity();
    double perimeter, a, b, c;
    for(int i=start; i <= end - 2; i++) {
        for(int j=i + 1; j <= end - 1; j++) {
            for(int k=j + 1; k <= end; k++) {
                a = distance(points[i], points[j]);
                b = distance(points[i], points[k]);
                c = distance(points[j], points[k]);
                if(a + b > c || b + c > a || a + c > b) {
                    perimeter = distance(points[i], points[j]) + distance(points[i], points[k]) + distance(points[j], points[k]);
                    if(min_perimeter > perimeter) {
                    min_perimeter = perimeter;
                    best_points[0] = points[i];
                    best_points[1] = points[j];
                    best_points[2] = points[k];
                }
                }
            }
        }
    }
    return min_perimeter;
}

double divide_in_half(int start, int end, pair<int, int>* points, pair<int, int>* best_points) {
    if(end - start < 6) {
        return brute_force(start, end, points, best_points);
    }
    int mid = (end + start)/2;

    int index_points = 0;
    int len = end - start;
    pair<int, int> points_y[len + 1];

    pair<int, int> best_points1[3];
    pair<int, int> best_points2[3];

    for(int i= 0; i < 3; i++) {
        best_points1[i] = make_pair(0, 0);
        best_points2[i] = make_pair(0, 0);
    }
    double perimeter;
    double perimeter1 = divide_in_half(start, mid, points, best_points1);
    double perimeter2 = divide_in_half(mid + 1, end, points, best_points2);

    if(perimeter1 > perimeter2) {
        perimeter = perimeter2;
        for(int i=0; i<3; i++) {
            best_points[i] = best_points2[i];
        }
    }
    else {
        perimeter = perimeter1;
        for(int i=0; i<3; i++) {
            best_points[i] = best_points1[i];
        }
    }

    int left = points[mid].first - perimeter/2 - 1;
    int right = points[mid].first + perimeter/2 + 1;

    for(int i = start; i <= end; i++) {
        if(points[i].first >= left && points[i].first <= right) {
            points_y[index_points] = points[i];
            index_points++;
        }
    }

    sort(points_y, points_y + index_points, sort_by_second);
    if(index_points < 3) {
        return perimeter;
    }
    int iterator = 0, max = 0;
    
    iterator = 1;
    double min_perimeter;
    double new_perimeter = perimeter;
    for(int i=0; i < index_points; i++) {
        max = points_y[i].second + new_perimeter/2 + 1;
        while(iterator < index_points && points_y[iterator].second <= max) {
            iterator++;
        }
        if(iterator - i > 2) {
            min_perimeter = brute_force(i, iterator - 1, points_y, best_points1);
            if(perimeter > min_perimeter) {
                perimeter = min_perimeter;
                for(int j=0; j<3; j++) {
                    best_points[j] = best_points1[j];
                }
            }
        }
    }

    return perimeter;
}

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    cout.tie(0);

    int n;
    cin >> n;

    int x, y;
    pair<int, int> points_x[n];
    pair<int, int> best_points[3];
    for(int i= 0; i < 3; i++) {
        best_points[i] = make_pair(0, 0);
    }
    for(int i=0; i < n; i++) {
        cin >> x >> y;
        points_x[i] = make_pair(x,y);
    }

    sort(points_x, points_x + n);
    divide_in_half(0, n -1, points_x, best_points);
    for(int i=0; i< 3; i++) {
        cout << best_points[i].first << " " << best_points[i].second << "\n";
    }
    return 0;

}