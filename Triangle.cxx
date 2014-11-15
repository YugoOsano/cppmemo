// This sample is downloaded from:
//  http://www.vtk.org/Wiki/VTK/Examples/Cxx/GeometricObjects/Triangle
// and modified.

#include <iostream>

#include <vtkSmartPointer.h>
#include <vtkTriangle.h>
#include <vtkXMLPolyDataWriter.h>
#include <vtkCellArray.h>
#include <vtkPolyData.h>

void PrintTriangle(vtkTriangle* Triangle);

int main(int, char *[])
{
  
  vtkSmartPointer<vtkTriangle> triangle = vtkSmartPointer<vtkTriangle>::New();
  triangle->GetPoints()->SetPoint(0, 1.0, 0.0, 0.0);
  triangle->GetPoints()->SetPoint(1, 0.0, 0.0, 0.0);
  triangle->GetPoints()->SetPoint(2, 0.0, 1.0, 0.5);
  
  PrintTriangle(triangle);
  
  //--Addition(Compute Normal)--
  double x0[3], x1[3], x2[3], n[3];
  triangle->GetPoints()->GetPoint(0, x0);
  triangle->GetPoints()->GetPoint(1, x1);
  triangle->GetPoints()->GetPoint(2, x2);
  triangle->ComputeNormal(x0, x1, x2, n);
  
  std::cout << n[0] << "\t" 
	    << n[1] << "\t"
	    << n[2] << std::endl;
  //--Addition end--
  return EXIT_SUCCESS;
}

void PrintTriangle(vtkTriangle* triangle)
{
  double p[3];
  for(unsigned int i = 0; i < 3; i++)
    {
    triangle->GetPoints()->GetPoint(i, p);
    cout << p[0] << " " << p[1] << " " << p[2] <<endl;
    }
}
