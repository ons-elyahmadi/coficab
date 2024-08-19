import { Component, OnInit } from '@angular/core';
import { NavigationEnd, Router } from '@angular/router';
import { filter } from 'rxjs/operators';
import { ApiService } from '../api.service';
import { AuthService } from '../auth.service';

@Component({
  selector: 'app-agent-dashboard',
  templateUrl: './agent-dashboard.component.html',
  styleUrls: ['./agent-dashboard.component.css']
})
export class AgentDashboardComponent  implements OnInit {
  showVideo: boolean = false; // Initialize with false to hide video by default

  constructor(
    private apiService: ApiService,
    private router: Router,
    private authService: AuthService // Inject AuthService
  ) {}

  ngOnInit(): void {
    // Check the current URL on component initialization
    this.checkVideoDisplay();

    // Update showVideo whenever navigation ends
    this.router.events.pipe(
      filter(event => event instanceof NavigationEnd)
    ).subscribe(() => {
      this.checkVideoDisplay();
    });
  }

  checkVideoDisplay() {
    // Show video only when URL is '/dashboard'
    this.showVideo = this.router.url === '/dashboard';
  }

  logout() {
    this.apiService.logout().subscribe(
      response => {
        console.log('Logout successful:', response);
        this.authService.logout(); // Use AuthService to handle logout logic
        this.router.navigate(['/login']); // Redirect to login page after logout
      },
      error => {
        console.error('Logout failed:', error);
      }
    );
  }
}