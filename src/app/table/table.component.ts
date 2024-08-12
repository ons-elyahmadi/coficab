import { Component, OnInit } from '@angular/core';
import { ApiService, TableData } from '../api.service'; 

@Component({
  selector: 'app-table',
  templateUrl: './table.component.html',
  styleUrls: ['./table.component.css']
})
export class TableComponent implements OnInit {
  tables: string[] = [];
  selectedTables: string[] = [];
  tableData: { [key: string]: TableData[] } = {};

  constructor(private tableService: ApiService) {}

  ngOnInit(): void {
    this.tableService.getTables().subscribe((tables) => {
      this.tables = tables;
    });
  }

  onTableSelect(selectedTables: string[]): void {
    this.selectedTables = selectedTables;

    selectedTables.forEach((tableName) => {
      if (!this.tableData[tableName]) {
        this.tableService.getTableData(tableName).subscribe((data) => {
          this.tableData[tableName] = data;
        });
      }
    });
  }

  getColumns(tableName: string): string[] {
    return this.tableData[tableName]?.length ? Object.keys(this.tableData[tableName][0]) : [];
  }
}
