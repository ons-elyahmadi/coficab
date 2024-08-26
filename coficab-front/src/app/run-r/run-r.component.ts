import { Component, OnInit } from '@angular/core';
import { ApiService } from '../api.service';
import { MatSnackBar } from '@angular/material/snack-bar';

@Component({
  selector: 'app-run-r',
  templateUrl: './run-r.component.html',
  styleUrls: ['./run-r.component.css']
})
export class RunRComponent implements OnInit {

  constructor(private apiService: ApiService, private _snackBar: MatSnackBar) { }

  ngOnInit(): void {
  }

  runTask(stepNumber: number, status: 'success' | 'failed'): void {
    const button = document.getElementById(`step${stepNumber}Button`);
    if (button) {
      this.setTaskStatus(stepNumber, 'in-progress');
      setTimeout(() => {
        this.setTaskStatus(stepNumber, status);
      }, 3000); // Simulate task completion or failure in 3 seconds
    }
  }

  setTaskStatus(stepNumber: number, status: string): void {
    const button = document.getElementById(`step${stepNumber}Button`);
    if (button) {
      button.classList.remove('not-started', 'in-progress', 'success', 'failed');
      button.classList.add(status);
    }
  }

  runRScript(): void {
    this.apiService.runRScript().subscribe({
      next: (response: any) => {
        this._snackBar.open('Scraping script executed successfully', 'Close', { duration: 3000, panelClass: ['snack-bar-success'] });
        this.runTask(1, 'success');
      },
      error: (error) => {
        this._snackBar.open('Failed to execute Scraping script', 'Close', { duration: 3000, panelClass: ['snack-bar-error'] });
        this.runTask(1, 'failed');
      }
    });
  }

  runCleaningScripts(): void {
    this.apiService.runRCScript().subscribe({
      next: (response: any) => {
        this._snackBar.open('Cleaning scripts executed successfully', 'Close', { duration: 3000, panelClass: ['snack-bar-success'] });
        this.runTask(2, 'success');
      },
      error: (error) => {
        this._snackBar.open('Failed to execute cleaning scripts', 'Close', { duration: 3000, panelClass: ['snack-bar-error'] });
        this.runTask(2, 'failed');
      }
    });
  }

  runRFScript(): void {
    this.apiService.runRfScript().subscribe({
      next: (response: any) => {
        this._snackBar.open('Forecast script executed successfully', 'Close', { duration: 3000, panelClass: ['snack-bar-success'] });
        this.runTask(3, 'success');
      },
      error: (error) => {
        this._snackBar.open('Failed to execute Forecast script', 'Close', { duration: 3000, panelClass: ['snack-bar-error'] });
        this.runTask(3, 'failed');
      }
    });
  }

  runSSIR(): void {
    this.apiService.runSSIR().subscribe({
      next: (response: any) => {
        this._snackBar.open('SSIR executed successfully', 'Close', { duration: 3000, panelClass: ['snack-bar-success'] });
        this.runTask(4, 'success');
      },
      error: (error) => {
        this._snackBar.open('Failed to execute SSIR', 'Close', { duration: 3000, panelClass: ['snack-bar-error'] });
        this.runTask(4, 'failed');
      }
    });
  }
}
