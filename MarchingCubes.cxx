/**
 * extract vtkTriangle from vtkPolyData obtained by 
 * vtkMarchingCubes algorithm. 
 * code is added to the vtk example:
 * http://www.vtk.org/Wiki/VTK/Examples/Cxx/Modelling/MarchingCubes
 */
#include <vtkVersion.h>
#include <vtkSmartPointer.h>
#include <vtkMarchingCubes.h>
#include <vtkVoxelModeller.h>
#include <vtkSphereSource.h>
#include <vtkImageData.h>
#include <vtkDICOMImageReader.h>

#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkRenderWindowInteractor.h>
#include <vtkRenderWindow.h>
#include <vtkRenderer.h>

#include <vtkPolyData.h>
#include <vtkCell.h>
#include <vtkTriangle.h>
#include <vtkCellArray.h>
#include <vtkIdList.h>
#include <iostream>

int main(int argc, char *argv[])
{
  vtkSmartPointer<vtkImageData> volume =
    vtkSmartPointer<vtkImageData>::New();
  double isoValue;
  if (argc < 3)
    {
    vtkSmartPointer<vtkSphereSource> sphereSource = 
      vtkSmartPointer<vtkSphereSource>::New();
    sphereSource->SetPhiResolution(20);
    sphereSource->SetThetaResolution(20);
    //sphereSource->SetPhiResolution(40);
    //sphereSource->SetThetaResolution(40);
    sphereSource->Update();
    
    double bounds[6];
    sphereSource->GetOutput()->GetBounds(bounds);
    for (unsigned int i = 0; i < 6; i++)
      std::cout << bounds[i] << "\t" ;
    std::cout << std::endl;

    for (unsigned int i = 0; i < 6; i += 2)
      {
      double range = bounds[i+1] - bounds[i];
      bounds[i]   = bounds[i] - .1 * range;
      bounds[i+1] = bounds[i+1] + .1 * range;
      }
    vtkSmartPointer<vtkVoxelModeller> voxelModeller = 
      vtkSmartPointer<vtkVoxelModeller>::New();
    voxelModeller->SetSampleDimensions(50,50,50);
    //voxelModeller->SetSampleDimensions(100,100,100);
    voxelModeller->SetModelBounds(bounds);
    voxelModeller->SetScalarTypeToFloat();
    voxelModeller->SetMaximumDistance(.1);
    
    voxelModeller->SetInputConnection(sphereSource->GetOutputPort());
    voxelModeller->Update();
    isoValue = 0.5;
    volume->DeepCopy(voxelModeller->GetOutput());
    }
  else
    {
    vtkSmartPointer<vtkDICOMImageReader> reader =
      vtkSmartPointer<vtkDICOMImageReader>::New();
    reader->SetDirectoryName(argv[1]);
    reader->Update();
    volume->DeepCopy(reader->GetOutput());
    isoValue = atof(argv[2]);
    }

  vtkSmartPointer<vtkMarchingCubes> surface = 
    vtkSmartPointer<vtkMarchingCubes>::New();

#if VTK_MAJOR_VERSION <= 5
  surface->SetInput(volume);
#else
  surface->SetInputData(volume);
#endif
  surface->ComputeNormalsOn();
  surface->SetValue(0, isoValue);

  //--addition --
  surface->Update();

  vtkSmartPointer<vtkPolyData> surfPoly = 
    vtkSmartPointer<vtkPolyData>::New();
  vtkSmartPointer<vtkCellArray> cellary = 
    vtkSmartPointer<vtkCellArray>::New();

  
  surfPoly = surface->GetOutput();
  cellary  = surfPoly->GetPolys();
  int npoly = surfPoly->GetNumberOfCells();
  int n = cellary->GetNumberOfCells();

  std::cout << "npoly: " << npoly << std::endl;
  
  for(int i = 0; i < npoly; i++){
    
    vtkSmartPointer<vtkTriangle> triangle = 
      vtkSmartPointer<vtkTriangle>::New();
    triangle->ShallowCopy(surfPoly->GetCell(i));
  
    double x[3];
    triangle->GetPoints()->GetPoint(0, x);

    std::cout << "triangle: " << i << "\t"
	      << triangle->GetNumberOfPoints() << "\t"
	      << x[0] << "\t" << x[1] << "\t" << x[2] << std::endl;
  }
    /*
  vtkSmartPointer<vtkIdList> idlist = 
    vtkSmartPointer<vtkIdList>::New();

  int n_nextcell; 
  n_nextcell = cellary->GetNextCell(idlist);
  n_nextcell = cellary->GetNextCell(idlist);
  n_nextcell = cellary->GetNextCell(idlist);

  std::cout << "nextcell: " << n_nextcell << std::endl;
  std::cout << "idlist: " 
	    << idlist[0] << "\t"  << std::endl;
  
  int n_get = idlist->GetId(0);
  std::cout << "n_get: " 
	    <<  idlist->GetId(0) << "\t" 
	    <<  idlist->GetId(1) << "\t" 
	    <<  idlist->GetId(2) << "\t" << std::endl;
    */
  //--end--
  vtkSmartPointer<vtkRenderer> renderer = 
    vtkSmartPointer<vtkRenderer>::New();
  renderer->SetBackground(.1, .2, .3);

  vtkSmartPointer<vtkRenderWindow> renderWindow = 
    vtkSmartPointer<vtkRenderWindow>::New();
  renderWindow->AddRenderer(renderer);
  vtkSmartPointer<vtkRenderWindowInteractor> interactor = 
    vtkSmartPointer<vtkRenderWindowInteractor>::New();
  interactor->SetRenderWindow(renderWindow);

  vtkSmartPointer<vtkPolyDataMapper> mapper = 
    vtkSmartPointer<vtkPolyDataMapper>::New();
  mapper->SetInputConnection(surface->GetOutputPort());
  mapper->ScalarVisibilityOff();

  vtkSmartPointer<vtkActor> actor = 
    vtkSmartPointer<vtkActor>::New();
  actor->SetMapper(mapper);

  renderer->AddActor(actor);

  renderWindow->Render();
  interactor->Start();
  return EXIT_SUCCESS;
}
