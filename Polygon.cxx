// this example is from:
// http://www.vtk.org/Wiki/VTK/Examples/Cxx/GeometricObjects/Polygon
// and modified for practice of conversion between vtkCell and vtkPolyData

#include <vtkVersion.h>
#include <vtkSmartPointer.h>
#include <vtkPolygon.h>
#include <vtkCellArray.h>
#include <vtkPolyData.h>
#include <vtkPolyDataMapper.h>
#include <vtkActor.h>
#include <vtkRenderWindow.h>
#include <vtkRenderer.h>
#include <vtkRenderWindowInteractor.h>

int main(int, char *[])
{
  // Setup four points
  vtkSmartPointer<vtkPoints> points =
    vtkSmartPointer<vtkPoints>::New();
  points->InsertNextPoint(0.0, 0.0, 0.0);
  points->InsertNextPoint(1.0, 0.0, 0.0);
  points->InsertNextPoint(1.0, 1.0, 0.0);
  points->InsertNextPoint(0.0, 1.0, 0.0);
  
  // Create the polygon
  vtkSmartPointer<vtkPolygon> polygon =
    vtkSmartPointer<vtkPolygon>::New();
  polygon->GetPointIds()->SetNumberOfIds(4); //make a quad
  polygon->GetPointIds()->SetId(0, 0);
  polygon->GetPointIds()->SetId(1, 1);
  polygon->GetPointIds()->SetId(2, 2);
  polygon->GetPointIds()->SetId(3, 3);

  //--Addition (2nd polygon) --
  // Setup other four points
  points->InsertNextPoint(0.0, 0.0, 1.0);
  points->InsertNextPoint(1.0, 0.0, 1.0);
  points->InsertNextPoint(2.0, 1.0, 1.0);
  points->InsertNextPoint(1.0, 1.0, 1.0);
  
  // Create the polygon
  vtkSmartPointer<vtkPolygon> polygon2 =
    vtkSmartPointer<vtkPolygon>::New();
  polygon2->GetPointIds()->SetNumberOfIds(4); //make a quad
  polygon2->GetPointIds()->SetId(0, 4);
  polygon2->GetPointIds()->SetId(1, 5);
  polygon2->GetPointIds()->SetId(2, 6);
  polygon2->GetPointIds()->SetId(3, 7);
  //--Addition end --

  // Add the polygon to a list of polygons
  vtkSmartPointer<vtkCellArray> polygons =
    vtkSmartPointer<vtkCellArray>::New();
  polygons->InsertNextCell(polygon);
  polygons->InsertNextCell(polygon2);// Addition

  // Create a PolyData
  vtkSmartPointer<vtkPolyData> polygonPolyData =
    vtkSmartPointer<vtkPolyData>::New();
  polygonPolyData->SetPoints(points);
  polygonPolyData->SetPolys(polygons);

  //--Addition(extract vtkCell from PolyData)--
  vtkSmartPointer<vtkPolygon> polygon3 =
    vtkSmartPointer<vtkPolygon>::New();
  // GetCell; choose 0 or 1 to see different quadrilateral
  polygon3->DeepCopy(polygonPolyData->GetCell(1));

  vtkSmartPointer<vtkCellArray> polygons2 =
    vtkSmartPointer<vtkCellArray>::New();
  polygons2->InsertNextCell(polygon3);

  vtkSmartPointer<vtkPolyData> polygonPolyData2 =
    vtkSmartPointer<vtkPolyData>::New();
  polygonPolyData2->SetPoints(points);
  polygonPolyData2->SetPolys(polygons2);
  //--Addition end--

  // Create a mapper and actor
  vtkSmartPointer<vtkPolyDataMapper> mapper =
    vtkSmartPointer<vtkPolyDataMapper>::New();
#if VTK_MAJOR_VERSION <= 5
  mapper->SetInput(polygonPolyData2);//modified
#else
  mapper->SetInputData(polygonPolyData2);//modified
#endif

  vtkSmartPointer<vtkActor> actor =
    vtkSmartPointer<vtkActor>::New();
  actor->SetMapper(mapper);

  // Visualize
  vtkSmartPointer<vtkRenderer> renderer =
    vtkSmartPointer<vtkRenderer>::New();
  vtkSmartPointer<vtkRenderWindow> renderWindow =
    vtkSmartPointer<vtkRenderWindow>::New();
  renderWindow->AddRenderer(renderer);
  vtkSmartPointer<vtkRenderWindowInteractor> renderWindowInteractor =
    vtkSmartPointer<vtkRenderWindowInteractor>::New();
  renderWindowInteractor->SetRenderWindow(renderWindow);

  renderer->AddActor(actor);
  renderer->SetBackground(.5,.3,.31); // Background color salmon

  renderWindow->Render();
  renderWindowInteractor->Start();

  return EXIT_SUCCESS;
}
