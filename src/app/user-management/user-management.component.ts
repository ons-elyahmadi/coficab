import { Component, OnInit } from '@angular/core';
import { ApiService } from '../api.service';
import { MatSnackBar } from '@angular/material/snack-bar';

@Component({
  selector: 'app-user-management',
  templateUrl: './user-management.component.html',
  styleUrls: ['./user-management.component.css']
})
export class UserManagementComponent implements OnInit {
  selected:boolean=true;
  
  users: any[] = [];
  selectedUser: any = {
    email: '',
    password: '',
    ROLE: '',
    username: ''
  };
  errorMessage: string = '';

  constructor(private apiService: ApiService, private _snackBar: MatSnackBar) { }

  showAlert(message: string, action: string) {
    this._snackBar.open(message, action, {
      duration: 2000,
    });
  }

  ngOnInit(): void {
    this.loadUsers();
  }

  loadUsers(): void {
    
    this.apiService.getUsers().subscribe(
      data => {
        this.users = data;
      },
      error => {
        console.error(error);
        this.errorMessage = 'Failed to load users';
      }
    );
  }

  selectUser(user: any): void {
    this.selected=false;
    this.selectedUser = { ...user };
  }

  saveUser(): void {
    if (this.selectedUser.id) {
      this.selected=true;
      this.apiService.updateUser(
        this.selectedUser.id, 
        this.selectedUser.email, 
        this.selectedUser.password, 
        this.selectedUser.ROLE, 
        this.selectedUser.username
      ).subscribe(
        () => {
          this.loadUsers();
          this.selectedUser = { email: '', password: '', ROLE: '', username: '' };
          this.showAlert('User updated successfully', 'Close');
        },
        error => {
          console.error(error);
          this.errorMessage = 'Failed to update user';
        }
      );
    } else {
      this.apiService.signup(
        this.selectedUser.email, 
        this.selectedUser.password, 
        this.selectedUser.ROLE, 
        this.selectedUser.username
      ).subscribe(
        () => {
          this.loadUsers();
          this.selectedUser = { email: '', password: '', ROLE: '', username: '' };
          this.showAlert('User added successfully', 'Close');
        },
        error => {
          console.error(error);
          this.errorMessage = 'Failed to add user';
        }
      );
    }
  }

  deleteUser(id: number): void {
    this.selected=true;
    this.apiService.deleteUser(id).subscribe(
      () => {
        this.loadUsers();
        this.selectedUser = { email: '', password: '', ROLE: '', username: '' };
        this.showAlert('User deleted successfully', 'Close');
      },
      error => {
        console.error(error);
        this.errorMessage = 'Failed to delete user';
      }
    );
  }

  clearForm(): void {
    this.selected=true;
    this.selectedUser = { email: '', password: '', ROLE: '', username: '' };
  }
}
