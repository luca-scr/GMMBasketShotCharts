#include <Rcpp.h>
// [[Rcpp::plugins("cpp11")]]
using namespace Rcpp;

// Function to check if a point is on a line segment
bool is_on_segment(double px, double py, 
                   double x1, double y1, 
                   double x2, double y2) 
{
  bool collinear = (y2 - y1) * (px - x1) == (py - y1) * (x2 - x1);
  bool within_bounds = (px >= std::min(x1, x2) && px <= std::max(x1, x2)) &&
    (py >= std::min(y1, y2) && py <= std::max(y1, y2));
  return collinear && within_bounds;
}

// Check if a point lies inside a polygon. 
// The ray-casting algorithm is used, which involves drawing a 
// horizontal ray from the point in question and counting how many 
// times it intersects the edges of the polygon. A point is considered
// strictly inside if the number of intersections is odd. 
// 
// Arguments:
// point: vector of (x,y) coordinates
// polygon: two-column matrix of (x,y) coordinates for the (closed) polygon
// boundary: logical, if TRUE allows the point to lie on the boundary
//
// [[Rcpp::export]]
bool point_in_polygon(NumericVector point, 
                      NumericMatrix polygon,
                      bool boundary = true) 
{
  if (point.size() != 2) 
  {
    stop("Point must have exactly two coordinates.");
  }
  
  int n = polygon.nrow();
  
  double x = point[0];
  double y = point[1];
  
  int intersections = 0;
  
  for (int i = 0; i < n; ++i) 
  {
    double x1 = polygon(i, 0); // x-coordinate of vertex i
    double y1 = polygon(i, 1); // y-coordinate of vertex i
    double x2 = polygon((i + 1) % n, 0); // x-coordinate of vertex (i+1)
    double y2 = polygon((i + 1) % n, 1); // y-coordinate of vertex (i+1)
    
    // Check if the point lies on the edge
    if (is_on_segment(x, y, x1, y1, x2, y2)) 
    {
      return boundary; // Return TRUE if boundary is allowed
    }
    
    // Ray-casting check for intersection
    if ((y > std::min(y1, y2)) && (y <= std::max(y1, y2)) && (x <= std::max(x1, x2))) 
    {
      double x_intersection = (y - y1) * (x2 - x1) / (y2 - y1) + x1;
      if (x1 == x2 || x <= x_intersection) 
      {
        intersections++;
      }
    }
  }
  
  return intersections % 2 == 1; // Odd intersections mean inside
}